defmodule RFC3339 do
  defstruct year: nil, month: nil, day: nil,
            hour: nil, min: nil, sec: nil, usec: nil,
            tz_offset: nil 

  def parse(dt) do
    case :rfc3339.parse(dt) do
      {:ok, {date, time, usec, tz}} ->
        dt = %RFC3339{}
        dt = insert_date(dt, date)
        dt = insert_time(dt, time)
        dt = insert_usec(dt, usec)
        insert_tz(dt, tz)
      {:error, error} ->
        {:error, error}
    end
  end

  def format(dt), do: :rfc3339.format(dt)

  defp insert_date(dt, {year, month, day}) do
    %{ dt | :year => year, :month => month, :day => day }
  end

  defp insert_time(dt, {hour, min, sec}) do
    %{ dt | :hour => hour, :min => min, :sec => sec }
  end

  defp insert_usec(dt, :undefined), do: dt
  defp insert_usec(dt, usec), do: %{ dt |  :usec => usec }

  defp insert_tz(dt, :undefined), do: dt
  defp insert_tz(dt, offset), do: %{ dt | :tz_offset => offset }
end

