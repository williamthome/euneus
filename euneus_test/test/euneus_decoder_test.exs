defmodule EuneusTest.EuneusDecoderTest do
  use ExUnit.Case, async: true

  test "numbers" do
    assert_fail_with("-", :unexpected_end)
    assert_fail_with("--1", {:invalid_byte, ?-})
    assert_fail_with("01", {:invalid_byte, ?1})
    assert_fail_with(".1", {:invalid_byte, ?.})
    assert_fail_with("1.", :unexpected_end)
    assert_fail_with("1e", :unexpected_end)
    assert_fail_with("1.0e+", :unexpected_end)
    assert_fail_with("1e999", {:unexpected_sequence, "1.0e999"})

    assert decode("0") == 0
    assert decode("1") == 1
    assert decode("-0") == 0
    assert decode("-1") == -1
    assert decode("0.1") == 0.1
    assert decode("-0.1") == -0.1
    assert decode("0e0") == 0
    assert decode("0E0") == 0
    assert decode("1e0") == 1
    assert decode("1E0") == 1
    assert decode("1.0e0") == 1.0
    assert decode("1e+0") == 1
    assert decode("1.0e+0") == 1.0
    assert decode("0.1e1") == 0.1e1
    assert decode("0.1e-1") == 0.1e-1
    assert decode("99.99e99") == 99.99e99
    assert decode("-99.99e-99") == -99.99e-99
    assert decode("123456789.123456789e123") == 123_456_789.123456789e123
  end

  test "strings" do
    assert_fail_with(~s("), :unexpected_end)
    assert_fail_with(~s("\\"), :unexpected_end)
    assert_fail_with(~s("\\k"), {:invalid_byte, ?k})
    assert_fail_with(<<?\", 0x80, ?\">>, {:invalid_byte, 0x80})
    assert_fail_with(~s("\\u2603\\"), :unexpected_end)
    assert_fail_with(~s("Here's a snowman for you: ☃. Good day!), :unexpected_end)
    assert_fail_with(~s("𝄞), :unexpected_end)
    assert_fail_with(~s(\u001F), {:invalid_byte, 31})
    assert_fail_with(~s("\\ud8aa\\udcxx"), {:unexpected_sequence, "\\ud8aa\\udcxx"})
    assert_fail_with(~s("\\ud8aa\\uda00"), {:unexpected_sequence, "\\ud8aa\\uda00"})
    assert_fail_with(~s("\\uxxxx"), {:unexpected_sequence, "\\uxxxx"})

    assert decode(~s("\\"\\\\\\/\\b\\f\\n\\r\\t")) == ~s("\\/\b\f\n\r\t)
    assert decode(~s("\\u2603")) == "☃"
    assert decode(~s("\\u2028\\u2029")) == "\u2028\u2029"
    assert decode(~s("\\uD834\\uDD1E")) == "𝄞"
    assert decode(~s("\\uD834\\uDD1E")) == "𝄞"
    assert decode(~s("\\uD799\\uD799")) == "힙힙"
    assert decode(~s("✔︎")) == "✔︎"
  end

  test "objects" do
    assert_fail_with("{", :unexpected_end)
    assert_fail_with("{,", {:invalid_byte, ?,})
    assert_fail_with(~s({"foo"}), {:invalid_byte, ?}})
    assert_fail_with(~s({"foo": "bar",}), {:invalid_byte, ?}})

    assert decode("{}") == %{}
    assert decode(~s({"foo": "bar"})) == %{"foo" => "bar"}
    assert decode(~s({"foo"  : "bar"})) == %{"foo" => "bar"}

    expected = %{"foo" => "bar", "baz" => "quux"}
    assert decode(~s({"foo": "bar", "baz": "quux"})) == expected

    expected = %{"foo" => %{"bar" => "baz"}}
    assert decode(~s({"foo": {"bar": "baz"}})) == expected
  end

  test "copying strings on decode" do
    assert decode("{}", %{value: :copy}) == %{}
    as = :binary.copy("a", 101)
    bs = :binary.copy("b", 102)

    # Copy decode, copies the key
    assert [{key, value}] =
             :maps.to_list(decode(~s({"#{as}": "#{bs}"}), %{value: :copy, key: :copy}))

    assert key == as
    assert value == bs
    assert :binary.referenced_byte_size(key) == byte_size(as)
    assert :binary.referenced_byte_size(value) == byte_size(bs)

    # Regular decode references the original string
    assert [{key, value}] = :maps.to_list(decode(~s({"#{as}": "#{bs}"})))
    assert key == as
    assert value == bs
    assert :binary.referenced_byte_size(key) > byte_size(as) + byte_size(bs)
    assert :binary.referenced_byte_size(value) > byte_size(bs) + byte_size(bs)
  end

  test "arrays" do
    assert_fail_with("[", :unexpected_end)
    assert_fail_with("[,", {:invalid_byte, ?,})
    assert_fail_with("[1,]", {:invalid_byte, ?]})

    assert decode("[]") == []
    assert decode("[1, 2, 3]") == [1, 2, 3]
    assert decode(~s(["foo", "bar", "baz"])) == ["foo", "bar", "baz"]
    assert decode(~s([{"foo": "bar"}])) == [%{"foo" => "bar"}]
  end

  test "whitespace" do
    assert_fail_with("", :unexpected_end)
    assert_fail_with("    ", :unexpected_end)

    assert decode("  [  ]  ") == []
    assert decode("  {  }  ") == %{}

    assert decode("  [  1  ,  2  ,  3  ]  ") == [1, 2, 3]

    expected = %{"foo" => "bar", "baz" => "quux"}
    assert decode(~s(  {  "foo"  :  "bar"  ,  "baz"  :  "quux"  }  )) == expected
  end

  test "iodata" do
    body = String.split(~s([1,2,3,4]), "")
    expected = [1, 2, 3, 4]
    assert decode(body) == expected
  end

  defp assert_fail_with(string, error) do
    assert catch_error(decode(string)) == error
  end

  defp decode(json, opts \\ %{}) do
    :euneus.decode(json, opts)
  end
end
