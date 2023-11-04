defmodule EuneusTest.EuneusSmartDecoderTest do
  use ExUnit.Case, async: true

  test "numbers" do
    assert_fail_with("-", :unexpected_end_of_input)
    assert_fail_with("--1", {:unexpected_byte, "0x2D", 1})
    assert_fail_with("01", {:unexpected_byte, "0x31", 1})
    assert_fail_with(".1", {:unexpected_byte, "0x2E", 0})
    assert_fail_with("1.", :unexpected_end_of_input)
    assert_fail_with("1e", :unexpected_end_of_input)
    assert_fail_with("1.0e+", :unexpected_end_of_input)
    assert_fail_with("1e999", {:unexpected_sequence, "1e999", 0})

    assert decode!("0") == 0
    assert decode!("1") == 1
    assert decode!("-0") == 0
    assert decode!("-1") == -1
    assert decode!("0.1") == 0.1
    assert decode!("-0.1") == -0.1
    assert decode!("0e0") == 0
    assert decode!("0E0") == 0
    assert decode!("1e0") == 1
    assert decode!("1E0") == 1
    assert decode!("1.0e0") == 1.0
    assert decode!("1e+0") == 1
    assert decode!("1.0e+0") == 1.0
    assert decode!("0.1e1") == 0.1e1
    assert decode!("0.1e-1") == 0.1e-1
    assert decode!("99.99e99") == 99.99e99
    assert decode!("-99.99e-99") == -99.99e-99
    assert decode!("123456789.123456789e123") == 123_456_789.123456789e123
  end

  test "strings" do
    assert_fail_with(~s("), :unexpected_end_of_input)
    assert_fail_with(~s("\\"), :unexpected_end_of_input)
    assert_fail_with(~s("\\k"), {:unexpected_byte, "0x6B", 2})
    assert_fail_with(<<?\", 128, ?\">>, {:unexpected_byte, "0x80", 1})
    assert_fail_with(~s("\\u2603\\"), :unexpected_end_of_input)

    assert_fail_with(
      ~s("Here's a snowman for you: ☃. Good day!),
      :unexpected_end_of_input
    )

    assert_fail_with(~s("𝄞), :unexpected_end_of_input)
    assert_fail_with(~s(\u001F), {:unexpected_byte, "0x1F", 0})
    assert_fail_with(~s("\\ud8aa\\udcxx"), {:unexpected_sequence, "\\udcxx", 7})

    assert_fail_with(
      ~s("\\ud8aa\\uda00"),
      {:unexpected_sequence, "\\ud8aa\\uda00", 1}
    )

    assert_fail_with(~s("\\uxxxx"), {:unexpected_sequence, "\\uxxxx", 1})

    assert decode!(~s("\\"\\\\\\/\\b\\f\\n\\r\\t")) == ~s("\\/\b\f\n\r\t)
    assert decode!(~s("\\u2603")) == "☃"
    assert decode!(~s("\\u2028\\u2029")) == "\u2028\u2029"
    assert decode!(~s("\\uD834\\uDD1E")) == "𝄞"
    assert decode!(~s("\\uD834\\uDD1E")) == "𝄞"
    assert decode!(~s("\\uD799\\uD799")) == "힙힙"
    assert decode!(~s("✔︎")) == "✔︎"
  end

  test "objects" do
    assert_fail_with("{", :unexpected_end_of_input)
    assert_fail_with("{,", {:unexpected_byte, "0x2C", 1})
    assert_fail_with(~s({"foo"}), {:unexpected_byte, "0x7D", 6})
    assert_fail_with(~s({"foo": "bar",}), {:unexpected_byte, "0x7D", 14})

    assert decode!("{}") == %{}
    assert decode!(~s({"foo": "bar"})) == %{"foo" => "bar"}
    assert decode!(~s({"foo"  : "bar"})) == %{"foo" => "bar"}

    expected = %{"foo" => "bar", "baz" => "quux"}
    assert decode!(~s({"foo": "bar", "baz": "quux"})) == expected

    expected = %{"foo" => %{"bar" => "baz"}}
    assert decode!(~s({"foo": {"bar": "baz"}})) == expected
  end

  test "arrays" do
    assert_fail_with("[", :unexpected_end_of_input)
    assert_fail_with("[,", {:unexpected_byte, "0x2C", 1})
    assert_fail_with("[1,]", {:unexpected_byte, "0x5D", 3})

    assert decode!("[]") == []
    assert decode!("[1, 2, 3]") == [1, 2, 3]
    assert decode!(~s(["foo", "bar", "baz"])) == ["foo", "bar", "baz"]
    assert decode!(~s([{"foo": "bar"}])) == [%{"foo" => "bar"}]
  end

  test "whitespace" do
    assert_fail_with("", :unexpected_end_of_input)
    assert_fail_with("    ", :unexpected_end_of_input)

    assert decode!("  [  ]  ") == []
    assert decode!("  {  }  ") == %{}

    assert decode!("  [  1  ,  2  ,  3  ]  ") == [1, 2, 3]

    expected = %{"foo" => "bar", "baz" => "quux"}
    assert decode!(~s(  {  "foo"  :  "bar"  ,  "baz"  :  "quux"  }  )) == expected
  end

  test "iodata" do
    body = String.split(~s([1,2,3,4]), "")
    expected = [1, 2, 3, 4]
    assert decode!(body) == expected
  end

  defp assert_fail_with(string, error) do
    assert {:error, error} == decode(string)
  end

  defp decode!(json) do
    {:ok, x} = decode(json)
    x
  end

  defp decode(json) do
    :euneus.decode(json)
  end
end
