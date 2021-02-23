

# Module prometheus_format #
* [Description](#description)

Module that implements this behaviour can be used
as `foramt` parameter for exporters.

__This module defines the `prometheus_format` behaviour.__<br /> Required callback functions: `content_type/0`, `format/0`, `format/1`.

<a name="description"></a>

## Description ##

Built-in formats:
- [`prometheus_text_format`](prometheus_text_format.md)
- [`prometheus_protobuf_format`](prometheus_protobuf_format.md)

Callbacks:
- `content_type()` - should return content type of the format;
- `format()` - should format `default` regsitry;
- `format(Registry)` - should format `Registry`.