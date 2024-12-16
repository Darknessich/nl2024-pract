defmodule JSON do
  def decode(json) do
    json
    |> String.trim()
    |> parse_value()
  end

  defp parse_value("" <> rest) when rest == "", do: {:error, "Unexpected end of input"}

  defp parse_value("null" <> rest), do: {:ok, nil, String.trim_leading(rest)}
  defp parse_value("true" <> rest), do: {:ok, true, String.trim_leading(rest)}
  defp parse_value("false" <> rest), do: {:ok, false, String.trim_leading(rest)}

  defp parse_value(<<"\"", rest::binary>>) do
    parse_string(rest)
  end

  defp parse_value(<<char, _rest::binary>> = json) when char in ?0..?9 or char == ?- do
    parse_number(json)
  end

  defp parse_value(<<"[", rest::binary>>) do
    parse_array(rest)
  end

  defp parse_value(<<"{", rest::binary>>) do
    parse_object(rest)
  end

  defp parse_value(_), do: {:error, "Invalid JSON value"}

  defp parse_string(json) do
    case String.split(json, "\"", parts: 2) do
      [string, rest] -> {:ok, string, String.trim_leading(rest)}
      _ -> {:error, "Unterminated string"}
    end
  end

  defp parse_number(json) do
    case Regex.run(~r/^-?\d+(\.\d+)?([eE][+-]?\d+)?/, json) do
      [number | _] ->
        value =
          if String.contains?(number, ".") do
            String.to_float(number)
          else
            String.to_integer(number)
          end

        {:ok, value, String.trim_leading(String.replace_prefix(json, number, ""))}

      _ -> {:error, "Invalid number"}
    end
  end

  defp parse_array(json) do
    parse_elements(String.trim_leading(json), [])
  end

  defp parse_elements(<<"]", rest::binary>>, acc), do: {:ok, Enum.reverse(acc), String.trim_leading(rest)}

  defp parse_elements(json, acc) do
    case parse_value(json) do
      {:ok, value, rest} ->
        case String.trim_leading(rest) do
          <<",", rest::binary>> -> parse_elements(String.trim_leading(rest), [value | acc])
          <<"]", rest::binary>> -> {:ok, Enum.reverse([value | acc]), String.trim_leading(rest)}
          _ -> {:error, "Expected ',' or ']' in array"}
        end

      error -> error
    end
  end

  defp parse_object(json) do
    parse_key_values(String.trim_leading(json), %{})
  end

  defp parse_key_values(<<"}", rest::binary>>, acc), do: {:ok, acc, String.trim_leading(rest)}

  defp parse_key_values(json, acc) do
    with {:ok, key, rest1} <- parse_value(json),
         <<":", rest2::binary>> <- String.trim_leading(rest1),
         {:ok, value, rest3} <- parse_value(String.trim_leading(rest2)) do
      case String.trim_leading(rest3) do
        <<",", rest4::binary>> -> parse_key_values(String.trim_leading(rest4), Map.put(acc, key, value))
        <<"}", rest4::binary>> -> {:ok, Map.put(acc, key, value), String.trim_leading(rest4)}
        _ -> {:error, "Expected ',' or '}' in object"}
      end
    else
      _ -> {:error, "Invalid object"}
    end
  end
end

defmodule YAML do
  def encode(value, indent \\ 0) do
    case value do
      map when is_map(map) and map == %{} ->
        "{}"

      map when is_map(map) ->
        map
        |> Enum.map(fn {k, v} ->
          indent_spaces = String.duplicate(" ", indent)
          if is_map(v) or is_list(v) do
            "#{indent_spaces}#{k}:\n" <> encode(v, indent + 2)
          else
            "#{indent_spaces}#{k}: " <> encode(v, 0)
          end
        end)
        |> Enum.join("\n")

      list when is_list(list) and list == [] ->
        "[]"

      list when is_list(list) ->
        list
        |> Enum.map(fn v ->
          indent_spaces = String.duplicate(" ", indent)
          "#{indent_spaces}- " <> (encode(v, indent + 2) |> String.trim_leading())
        end)
        |> Enum.join("\n")

      str when is_binary(str) ->
        if String.downcase(str) in ["null", "false", "true", ""] or str =~ ~r/[\\]/ do
          String.duplicate(" ", indent) <> "\"#{str}\""
        else
          String.duplicate(" ", indent) <> str
        end

      num when is_number(num) ->
        String.duplicate(" ", indent) <> to_string(num)

      true -> String.duplicate(" ", indent) <> "true"
      false -> String.duplicate(" ", indent) <> "false"
      nil -> String.duplicate(" ", indent) <> "null"
    end
  end
end

defmodule Main do
  def main(args) do
    case args do
      [input_file, output_file] ->
        with {:ok, json} <- File.read(input_file),
             {:ok, parsed, _} <- JSON.decode(json),
             yaml <- YAML.encode(parsed),
             :ok <- File.write(output_file, yaml) do
          IO.puts("Successfully converted JSON to YAML and saved to #{output_file}")
        else
          {:error, reason} -> IO.puts("Error: #{reason}")
          _ -> IO.puts("An unknown error occurred")
        end

      _ ->
        IO.puts("Usage: elixir main.exs <input_json_file> <output_yaml_file>")
    end
  end
end

if function_exported?(System, :argv, 0) do
  Main.main(System.argv())
end
