defmodule :rfc3339_test do
  use ExUnit.Case

  test "decode 0000-01-01T00:00:00.0+00:00" do
    assert {:ok, {{0, 1, 1}, {0, 0, 0}, 0, 0}} = :rfc3339.parse("0000-01-01T00:00:00.0+00:00")
  end

  test "decode 9999-12-31T00:00:00.0+00:00" do
    assert {:ok, {{9999, 12, 31}, {0, 0, 0}, 0, 0}} = :rfc3339.parse("9999-12-31T00:00:00.0+00:00")
  end

  test "decode 1584-03-04T00:00:00.0+00:00" do
    assert {:ok, {{1584, 3, 4}, {0, 0, 0}, 0, 0}} = :rfc3339.parse("1584-03-04T00:00:00.0+00:00")
  end

  test "decode 1900-01-01T00:00:00.0+00:00" do
    assert {:ok, {{1900, 1, 1}, {0, 0, 0}, 0, 0}} = :rfc3339.parse("1900-01-01T00:00:00.0+00:00")
  end

  test "decode 2016-01-24T00:00:00.0+00:00" do
    assert {:ok, {{2016, 1, 24}, {0, 0, 0}, 0, 0}} = :rfc3339.parse("2016-01-24T00:00:00.0+00:00")
  end

  test "decode 1970-01-01T00:00:00Z" do
    assert {:ok, {{1970, 1, 1}, {0, 0, 0}, :undefined, :undefined}} = :rfc3339.parse("1970-01-01T00:00:00Z")
  end

  test "decode 1970-01-01T23:59:60Z" do
    assert {:ok, {{1970, 1, 1}, {23, 59, 60}, :undefined, :undefined}} = :rfc3339.parse("1970-01-01T23:59:60Z")
  end

  test "decode 1970-01-01T23:59:60.5Z" do
    assert {:ok, {{1970, 1, 1}, {23, 59, 60}, 500000, :undefined}} = :rfc3339.parse("1970-01-01T23:59:60.5Z")
  end

  test "decode 1970-01-01T23:59:60.55Z" do
    assert {:ok, {{1970, 1, 1}, {23, 59, 60}, 550000, :undefined}} = :rfc3339.parse("1970-01-01T23:59:60.55Z")
  end

  test "decode 1970-01-01T23:59:60.555555Z" do
    assert {:ok, {{1970, 1, 1}, {23, 59, 60}, 555555, :undefined}} = :rfc3339.parse("1970-01-01T23:59:60.555555Z")
  end

  test "decode 1970-01-01T23:59:60.5555554Z" do
    assert {:ok, {{1970, 1, 1}, {23, 59, 60}, 555555, :undefined}} = :rfc3339.parse("1970-01-01T23:59:60.5555554Z")
  end

  test "decode 1970-01-01T23:59:60.999999Z" do
    assert {:ok, {{1970, 1, 1}, {23, 59, 60}, 999999, :undefined}} = :rfc3339.parse("1970-01-01T23:59:60.999999Z")
  end

  test "decode 1970-01-01T23:59:60.9999999Z" do
    assert {:ok, {{1970, 1, 1}, {23, 59, 60}, 999999, :undefined}} = :rfc3339.parse("1970-01-01T23:59:60.9999999Z")
  end

  test "decode 1970-01-01T00:00:00+00:00" do
    assert {:ok, {{1970, 1, 1}, {0, 0, 0}, :undefined, 0}} = :rfc3339.parse("1970-01-01T00:00:00+00:00")
  end

  test "decode 1970-01-01T00:00:00-00:00" do
    assert {:ok, {{1970, 1, 1}, {0, 0, 0}, :undefined, 0}} = :rfc3339.parse("1970-01-01T00:00:00-00:00")
  end

  test "decode 1970-01-01T00:00:00+23:59" do
    assert {:ok, {{1970, 1, 1}, {0, 0, 0}, :undefined, 1439}} = :rfc3339.parse("1970-01-01T00:00:00+23:59")
  end

  test "decode 1970-01-01T23:59:60+00:00" do
    assert {:ok, {{1970, 1, 1}, {23, 59, 60}, :undefined, 0}} = :rfc3339.parse("1970-01-01T23:59:60+00:00")
  end

  test "decode 1970-01-01T23:59:60+23:59" do
    assert {:ok, {{1970, 1, 1}, {23, 59, 60}, :undefined, 1439}} = :rfc3339.parse("1970-01-01T23:59:60+23:59")
  end

  test "decode 1970-01-01T23:59:60-23:59" do
    assert {:ok, {{1970, 1, 1}, {23, 59, 60}, :undefined, -1439}} = :rfc3339.parse("1970-01-01T23:59:60-23:59")
  end

  test "decode 1979-06-21T22:20:03Z" do
    assert {:ok, {{1979, 6, 21}, {22, 20, 03}, :undefined, :undefined}} = :rfc3339.parse("1979-06-21T22:20:03Z")
  end

  test "decode 1979-06-21t22:20:03z" do
    assert {:ok, {{1979, 6, 21}, {22, 20, 03}, :undefined, :undefined}} = :rfc3339.parse("1979-06-21t22:20:03z")
  end

  test "decode 1979-06-21 22:20:03Z" do
    assert {:ok, {{1979, 6, 21}, {22, 20, 03}, :undefined, :undefined}} = :rfc3339.parse("1979-06-21 22:20:03Z")
  end

  test "decode 1979-06-21T22:20:03.9876543Z" do
    assert {:ok, {{1979, 6, 21}, {22, 20, 03}, 987654, :undefined}} = :rfc3339.parse("1979-06-21T22:20:03.9876543Z")
  end

  test "decode 1979-06-21T22:20:03+02:00" do
    assert {:ok, {{1979, 6, 21}, {22, 20, 03}, :undefined, 120}} = :rfc3339.parse("1979-06-21T22:20:03+02:00")
  end

  test "decode 1979-06-21T22:20:03.9876543+02:00" do
    assert {:ok, {{1979, 6, 21}, {22, 20, 03}, 987654, 120}} = :rfc3339.parse("1979-06-21T22:20:03.9876543+02:00")
  end

  test "encode 1979-06-21" do
    assert {:ok, "1979-06-21T00:00:00Z"} = :rfc3339.format({{1979, 6, 21}, {0, 0, 0}, :undefined, :undefined})
  end

  test "encode 1979-06-21T12:12:12Z" do
    assert {:ok, "1979-06-21T12:12:12Z"} = :rfc3339.format({{1979, 6, 21}, {12, 12, 12}, :undefined, :undefined})
  end

  test "encode 1979-06-21T12:12:12.120000Z" do
    assert {:ok, "1979-06-21T12:12:12.120000Z"} = :rfc3339.format({{1979, 6, 21}, {12, 12, 12}, 120000, :undefined})
  end

  test "encode 1979-06-21T12:12:12.000012Z" do
    assert {:ok, "1979-06-21T12:12:12.000012Z"} = :rfc3339.format({{1979, 6, 21}, {12, 12, 12}, 12, :undefined})
  end

  test "encode 1979-06-21T12:12:12+12:12" do
    assert {:ok, "1979-06-21T12:12:12+12:12"} = :rfc3339.format({{1979, 6, 21}, {12, 12, 12}, :undefined, 732})
  end

  test "encode 1979-06-21T12:12:12-12:12" do
    assert {:ok, "1979-06-21T12:12:12-12:12"} = :rfc3339.format({{1979, 6, 21}, {12, 12, 12}, :undefined, -732})
  end
end

defmodule RFC3339Test do
  use ExUnit.Case

  test "decode 0000-01-01T00:00:00.0+00:00" do
    assert %RFC3339{year: 0, month: 1, day: 1} = RFC3339.parse("0000-01-01T00:00:00.0+00:00")
  end

  test "decode 9999-12-31T00:00:00.0+00:00" do
    assert %RFC3339{year: 9999, month: 12, day: 31} = RFC3339.parse("9999-12-31T00:00:00.0+00:00")
  end

  test "decode 1584-03-04T00:00:00.0+00:00" do
    assert %RFC3339{year: 1584, month: 3, day: 4} = RFC3339.parse("1584-03-04T00:00:00.0+00:00")
  end

  test "decode 1900-01-01T00:00:00.0+00:00" do
    assert %RFC3339{year: 1900, month: 1, day: 1} = RFC3339.parse("1900-01-01T00:00:00.0+00:00")
  end

  test "decode 2016-01-24T00:00:00.0+00:00" do
    assert %RFC3339{year: 2016, month: 1, day: 24} = RFC3339.parse("2016-01-24T00:00:00.0+00:00")
  end

  test "decode 1970-01-01T00:00:00Z" do
    assert %RFC3339{hour: 0, min: 0, sec: 0} = RFC3339.parse("1970-01-01T00:00:00Z")
  end

  test "decode 1970-01-01T23:59:60Z" do
    assert %RFC3339{hour: 23, min: 59, sec: 60} = RFC3339.parse("1970-01-01T23:59:60Z")
  end

  test "decode 1970-01-01T23:59:60.5Z" do
    assert %RFC3339{hour: 23, min: 59, sec: 60, usec: 500000} = RFC3339.parse("1970-01-01T23:59:60.5Z")
  end

  test "decode 1970-01-01T23:59:60.55Z" do
    assert %RFC3339{hour: 23, min: 59, sec: 60, usec: 550000} = RFC3339.parse("1970-01-01T23:59:60.55Z")
  end

  test "decode 1970-01-01T23:59:60.555555Z" do
    assert %RFC3339{hour: 23, min: 59, sec: 60, usec: 555555} = RFC3339.parse("1970-01-01T23:59:60.555555Z")
  end

  test "decode 1970-01-01T23:59:60.5555554Z" do
    assert %RFC3339{hour: 23, min: 59, sec: 60, usec: 555555} = RFC3339.parse("1970-01-01T23:59:60.5555554Z")
  end

  test "decode 1970-01-01T23:59:60.999999Z" do
    assert %RFC3339{hour: 23, min: 59, sec: 60, usec: 999999} = RFC3339.parse("1970-01-01T23:59:60.999999Z")
  end

  test "decode 1970-01-01T23:59:60.9999999Z" do
    assert %RFC3339{hour: 23, min: 59, sec: 60, usec: 999999} = RFC3339.parse("1970-01-01T23:59:60.9999999Z")
  end

  test "decode 1970-01-01T00:00:00+00:00" do
    assert %RFC3339{hour: 0, min: 0, sec: 0, tz_offset: 0} = RFC3339.parse("1970-01-01T00:00:00+00:00")
  end

  test "decode 1970-01-01T00:00:00-00:00" do
    assert %RFC3339{hour: 0, min: 0, sec: 0, tz_offset: 0} = RFC3339.parse("1970-01-01T00:00:00-00:00")
  end

  test "decode 1970-01-01T00:00:00+23:59" do
    assert %RFC3339{hour: 0, min: 0, sec: 0, tz_offset: 1439} = RFC3339.parse("1970-01-01T00:00:00+23:59")
  end

  test "decode 1970-01-01T23:59:60+00:00" do
    assert %RFC3339{hour: 23, min: 59, sec: 60, tz_offset: 0} = RFC3339.parse("1970-01-01T23:59:60+00:00")
  end

  test "decode 1970-01-01T23:59:60+23:59" do
    assert %RFC3339{hour: 23, min: 59, sec: 60, tz_offset: 1439} = RFC3339.parse("1970-01-01T23:59:60+23:59")
  end

  test "decode 1970-01-01T23:59:60-23:59" do
    assert %RFC3339{hour: 23, min: 59, sec: 60, tz_offset: -1439} = RFC3339.parse("1970-01-01T23:59:60-23:59")
  end

  test "decode 1979-06-21T22:20:03Z" do
    assert %RFC3339{year: 1979, month: 6, day: 21, hour: 22, min: 20, sec: 3} = RFC3339.parse("1979-06-21T22:20:03Z")
  end

  test "decode 1979-06-21t22:20:03z" do
    assert %RFC3339{year: 1979, month: 6, day: 21, hour: 22, min: 20, sec: 3} = RFC3339.parse("1979-06-21t22:20:03z")
  end

  test "decode 1979-06-21 22:20:03Z" do
    assert %RFC3339{year: 1979, month: 6, day: 21, hour: 22, min: 20, sec: 3} = RFC3339.parse("1979-06-21 22:20:03Z")
  end

  test "decode 1979-06-21T22:20:03.9876543Z" do
    assert %RFC3339{year: 1979, month: 6, day: 21, hour: 22, min: 20, sec: 3, usec: 987654} = RFC3339.parse("1979-06-21T22:20:03.9876543Z")
  end

  test "decode 1979-06-21T22:20:03+02:00" do
    assert %RFC3339{year: 1979, month: 6, day: 21, hour: 22, min: 20, sec: 3, tz_offset: 120} = RFC3339.parse("1979-06-21T22:20:03+02:00")
  end

  test "decode 1979-06-21T22:20:03.9876543+02:00" do
    assert %RFC3339{year: 1979, month: 6, day: 21, hour: 22, min: 20, sec: 3, usec: 987654, tz_offset: 120} = RFC3339.parse("1979-06-21T22:20:03.9876543+02:00")
  end

  test "encode 1979-06-21" do
    assert {:ok, "1979-06-21T00:00:00Z"} = RFC3339.format(%RFC3339{year: 1979, month: 6, day: 21})
  end

  test "encode 1979-06-21T12:12:12Z" do
    assert {:ok, "1979-06-21T12:12:12Z"} = RFC3339.format(%RFC3339{year: 1979, month: 6, day: 21, hour: 12, min: 12, sec: 12})
  end

  test "encode 1979-06-21T12:12:12.120000Z" do
    assert {:ok, "1979-06-21T12:12:12.120000Z"} = RFC3339.format(%RFC3339{year: 1979, month: 6, day: 21, hour: 12, min: 12, sec: 12, usec: 120000})
  end

  test "encode 1979-06-21T12:12:12.000012Z" do
    assert {:ok, "1979-06-21T12:12:12.000012Z"} = RFC3339.format(%RFC3339{year: 1979, month: 6, day: 21,hour: 12, min: 12, sec: 12, usec: 12})
  end

  test "encode 1979-06-21T12:12:12+12:12" do
    assert {:ok, "1979-06-21T12:12:12+12:12"} = RFC3339.format(%RFC3339{year: 1979, month: 6, day: 21, hour: 12, min: 12, sec: 12, tz_offset: 732})
  end

  test "encode 1979-06-21T12:12:12-12:12" do
    assert {:ok, "1979-06-21T12:12:12-12:12"} = RFC3339.format(%RFC3339{year: 1979, month: 6, day: 21, hour: 12, min: 12, sec: 12, tz_offset: -732})
  end
end

defmodule :rfc3339_maps do
  use ExUnit.Case

  test "to_map 1979-06-21T12:12:12.12+12:12" do
    assert %{:year => 1979, :month => 6, :day => 21,
             :hour => 12, :min => 12, :sec => 12,
             :usec => 120000, :tz_offset => 732} = :rfc3339.to_map("1979-06-21T12:12:12.12+12:12")
  end

  test "encode 1979-06-21T12:12:12.12+12:12" do
    assert {:ok, "1979-06-21T12:12:12.120000+12:12"} = :rfc3339.format(%{:year => 1979, :month => 6, :day => 21,
                                                                         :hour => 12, :min => 12, :sec => 12,
                                                                         :usec => 120000, :tz_offset => 732})
  end

  test "to_map 1979-06-21T12:12:12Z" do
    assert %{:year => 1979, :month => 6, :day => 21,
             :hour => 12, :min => 12, :sec => 12} = :rfc3339.to_map("1979-06-21T12:12:12Z")
  end

  test "encode 1979-06-21T12:12:12Z" do
    assert {:ok, "1979-06-21T12:12:12Z"} = :rfc3339.format(%{:year => 1979, :month => 6, :day => 21,
                                                             :hour => 12, :min => 12, :sec => 12})
  end
end

defmodule :rfc3339_inttime do
  use ExUnit.Case

  test "to_time 1970-01-01T00:00:00Z" do
    assert {:ok, 0} = :rfc3339.to_time("1970-01-01T00:00:00Z", :micro_seconds)
  end

  test "encode 1970-01-01T00:00:00Z" do
    assert {:ok, "1970-01-01T00:00:00Z"} = :rfc3339.format(0, :micro_seconds)
  end

  test "to_time 1970-01-01T00:00:00.54321Z" do
    assert {:ok, 543210} = :rfc3339.to_time("1970-01-01T00:00:00.54321Z", :micro_seconds)
  end

  test "encode 1970-01-01T00:00:00.543210Z" do
    assert {:ok, "1970-01-01T00:00:00.543210Z"} = :rfc3339.format(543210, :micro_seconds)
  end

  test "to_time 1970-01-01T00:00:00.54321Z (ms)" do
    assert {:ok, 543} = :rfc3339.to_time("1970-01-01T00:00:00.54321Z", :milli_seconds)
  end

  test "encode 1970-01-01T00:00:00.543000Z (ms)" do
    assert {:ok, "1970-01-01T00:00:00.543000Z"} = :rfc3339.format(543, :milli_seconds)
  end

  test "to_time 1970-01-01T00:00:00.54321Z (ns)" do
    assert {:ok, 543210000} = :rfc3339.to_time("1970-01-01T00:00:00.54321Z", :nano_seconds)
  end

  test "encode 1970-01-01T00:00:00.543000Z (ns)" do
    assert {:ok, "1970-01-01T00:00:00.543000Z"} = :rfc3339.format(543000000, :nano_seconds)
  end

  test "to_time 1970-01-01T00:00:06.54321Z" do
    assert {:ok, 6543210} = :rfc3339.to_time("1970-01-01T00:00:06.54321Z", :micro_seconds)
  end

  test "encode 1970-01-01T00:00:06.543210Z" do
    assert {:ok, "1970-01-01T00:00:06.543210Z"} = :rfc3339.format(6543210, :micro_seconds)
  end

  test "to_time 1979-06-21T12:12:12Z" do
    assert {:ok, 298815132000000} = :rfc3339.to_time("1979-06-21T12:12:12Z", :micro_seconds)
  end

  test "encode 1979-06-21T12:12:12Z" do
    assert {:ok, "1979-06-21T12:12:12Z"} = :rfc3339.format(298815132000000, :micro_seconds)
  end

  test "to_time 1918-11-11T11:00:00Z" do
    assert {:ok, -1613826000000000} = :rfc3339.to_time("1918-11-11T11:00:00Z", :micro_seconds)
  end

  test "encode 1918-11-11T11:00:00Z" do
    assert {:ok, "1918-11-11T11:00:00Z"} = :rfc3339.format(-1613826000000000, :micro_seconds)
  end

  test "to_time 1918-11-11T11:00:00+02:00" do
    assert {:ok, -1613818800000000} = :rfc3339.to_time("1918-11-11T11:00:00+02:00", :micro_seconds)
  end

  test "encode 1918-11-11T11:00:00+02:00" do
    assert {:ok, "1918-11-11T13:00:00Z"} = :rfc3339.format(-1613818800000000, :micro_seconds)
  end
end

defmodule RFC3339Test.Errors do
  use ExUnit.Case

  test "badarg" do
    assert {:error, :badarg} = RFC3339.parse(1)
  end

  test "baddate" do
    assert {:error, :baddate} = RFC3339.parse("79-06-21T00:00:00Z")
  end

  test "badyear" do
    assert {:error, :badyear} = RFC3339.parse("xxxx-06-21T00:00:00Z")
  end

  test "badmonth" do
    assert {:error, :badmonth} = RFC3339.parse("1979-xx-21T00:00:00Z")
  end

  test "badday" do
    assert {:error, :badday} = RFC3339.parse("1979-06-xxT00:00:00Z")
  end

  test "badtime" do
    assert {:error, :badtime} = RFC3339.parse("1979-06-21-00:00:00Z")
  end

  test "badhour" do
    assert {:error, :badhour} = RFC3339.parse("1979-06-21Txx:00:00Z")
  end

  test "badmin" do
    assert {:error, :badminute} = RFC3339.parse("1979-06-21T00:xx:00Z")
  end

  test "badsec" do
    assert {:error, :badsecond} = RFC3339.parse("1979-06-21T00:00:xxZ")
  end

  test "badtz" do
    assert {:error, :badtimezone} = RFC3339.parse("1979-06-21T00:00:00x")
  end
end
