if Code.ensure_loaded?(ExUnitProperties) do
  defmodule EuneusTest.EuneusSmartPropertyTest do
    use ExUnit.Case, async: true
    use ExUnitProperties

    property "string roundtrip" do
      check all(string <- string(:printable)) do
        assert decode!(encode!(string)) == string
      end
    end

    property "integer roundtrip" do
      check all(integer <- integer()) do
        assert decode!(encode!(integer)) == integer
      end
    end

    property "float roundtrip" do
      check all(float <- float()) do
        assert decode!(encode!(float)) == float
      end
    end

    property "string-keyed objects roundrtip" do
      check all(json <- json(string(:printable))) do
        assert decode!(encode!(json)) == json
      end
    end

    defp json(keys) do
      null_term = :undefined
      simple = one_of([integer(), float(), string(:printable), boolean(), null_term])

      tree(simple, fn json ->
        one_of([list_of(json), map_of(keys, json)])
      end)
    end

    defp encode!(x) do
      {:ok, json} = :euneus.encode_to_binary(x)
      json
    end

    defp decode!(json) do
      {:ok, x} = :euneus.decode(json)
      x
    end
  end
end
