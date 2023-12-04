defmodule EuneusTest.EuneusSmartEncoderTest do
  use ExUnit.Case, async: true

  test "atom" do
    assert encode!(:null) == "null"
    assert encode!(true) == "true"
    assert encode!(false) == "false"
    assert encode!(:poison) == ~s("poison")
  end

  test "integer" do
    assert encode!(42) == "42"
  end

  test "float" do
    assert encode!(99.99) == "99.99"
    assert encode!(9.9e100) == "9.9e100"
  end

  test "binaries" do
    assert encode!("hello world") == ~s("hello world")
    assert encode!("hello\nworld") == ~s("hello\\nworld")
    assert encode!("\nhello\nworld\n") == ~s("\\nhello\\nworld\\n")

    assert encode!("\"") == ~s("\\"")
    assert encode!("\0") == ~s("\\u0000")
    assert encode!(<<31>>) == ~s("\\u001F")

    assert encode!("áéíóúàèìòùâêîôûãẽĩõũ") == ~s("áéíóúàèìòùâêîôûãẽĩõũ")
  end

  test "Map" do
    assert encode!(%{}) == "{}"
    assert encode!(%{"foo" => "bar"}) == ~s({"foo":"bar"})
    assert encode!(%{foo: :bar}) == ~s({"foo":"bar"})
    assert encode!(%{~c"foo" => :bar}) == ~s({"foo":"bar"})
    assert encode!(%{0 => 0}) == ~s({"0":0})

    multi_key_map = %{"foo" => "foo1", :foo => "foo2"}

    assert encode!(multi_key_map) == ~s({"foo":"foo2","foo":"foo1"})
  end

  test "list" do
    assert encode!([]) == "[]"
    assert encode!([1, 2, 3]) == "[1,2,3]"
  end

  test "throws error" do
    assert encode(<<0x80>>) == {:error, {:invalid_byte, ~s(0x80), <<128>>}}

    assert encode(<<?a, 208>>) ==
             {:error, {:invalid_byte, ~s(0xD0), <<97, 208>>}}
  end

  defp encode!(x) do
    {:ok, json} = encode(x)
    json
  end

  defp encode(x) do
    :euneus.encode_to_binary(x)
  end
end
