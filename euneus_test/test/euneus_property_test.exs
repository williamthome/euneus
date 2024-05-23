if Code.ensure_loaded?(ExUnitProperties) do
  defmodule EuneusTest.EuneusPropertyTest do
    use ExUnit.Case, async: true
    use ExUnitProperties

    property "string roundtrip" do
      check all(string <- string(:printable)) do
        assert decode(encode(string, %{})) == string
      end
    end

    property "integer roundtrip" do
      check all(integer <- integer()) do
        assert decode(encode(integer, %{})) == integer
      end
    end

    property "float roundtrip" do
      check all(float <- float()) do
        assert decode(encode(float, %{})) == float
      end
    end

    property "string-keyed objects roundrtip" do
      check all(json <- json(string(:printable))) do
        assert decode(encode(json, %{})) == json
      end
    end

    # TODO
    # property "html escaping" do
    #   check all(string <- string(:printable)) do
    #     encoded = encode(string, %{escape: :html})
    #     refute encoded =~ <<0x2028::utf8>>
    #     refute encoded =~ <<0x2029::utf8>>
    #     refute encoded =~ ~r"(?<!\\)/"
    #     assert decode(encoded) == string
    #   end
    # end

    # TODO
    # property "javascript escaping" do
    #   check all(string <- string(:printable)) do
    #     encoded = encode(string, %{escape: :javascript})
    #     refute encoded =~ <<0x2028::utf8>>
    #     refute encoded =~ <<0x2029::utf8>>
    #     assert decode(encoded) == string
    #   end
    # end

    defp json(keys) do
      null_term = :null
      simple = one_of([integer(), float(), string(:printable), boolean(), null_term])

      tree(simple, fn json ->
        one_of([list_of(json), map_of(keys, json)])
      end)
    end

    defp encode(x, opts) do
      :euneus.encode(x, opts)
    end

    defp decode(json, opts \\ %{}) do
      :euneus.decode(json, opts)
    end
  end
end
