# Named: Compile-Time Named Access to Structured Data in C++

> A C++20 header-only library for working with tuples, arrays, and bitsets using compile-time string tags.

![Header Only](https://img.shields.io/badge/header--only-yes-green)
![License: MIT](https://img.shields.io/badge/license-MIT-yellow)

## Overview

NamedTuple provides C++ types for named access to structured data, using string literals at compile time.  
It includes:

- `NamedTuple`: a tuple with name-based accessors
- `TaggedArray`: a fixed-size array with named elements
- `TaggedBitset`: a fixed-size bitset with named bits

## Requirements

- **C++20** compliant compiler:
    - GCC 10+, Clang 11+, MSVC 2019+
- **No external dependencies**
- Include the `Named/` directory in your project

### Single Include

There is a single include version at `single_include/Named.hpp`

I will attempt to keep the single include updated with the rest of the code, however, it is annoying so do not rely on this.

## Quick Start

### NamedTuple

```c++
#include <Named/NamedTuple.hpp>
#include <iostream>

namespace named = mguid;

int main() {
  named::NamedTuple<
    named::NamedType<"x", int>,
    named::NamedType<"y", int>
  > point;

  point.get<"x">() = 3;
  point.get<"y">() = 4;

  std::cout << "Point: (" << point.get<"x">() << ", " << point.get<"y">() << ")\n";
}
```

### Tagged Initialization with User-Defined Literals

```c++
#include <Named/NamedTuple.hpp>
#include <iostream>

namespace named = mguid;
using namespace mguid::literals;

int main() {
  named::NamedTuple coord{
    "lat"_named = 42.0,
    "lon"_named = -71.0
  };

  std::cout << "Latitude: " << coord.get<"lat">()
            << ", Longitude: " << coord.get<"lon">() << "\n";
}
```

### Tagged Array

```c++
#include <Named/TaggedArray.hpp>
#include <iostream>

namespace named = mguid;

int main() {
  named::TaggedArray<float, "width", "height"> dims{1024.0f, 768.0f};

  dims.set<"height">(800.0f);

  std::cout << "Width: " << dims.get<"width">()
            << ", Height: " << dims.get<"height">() << "\n";
}
```

### TaggedBitset

```c++
#include <Named/TaggedBitset.hpp>
#include <iostream>

namespace named = mguid;

int main() {
  named::TaggedBitset<"visible", "enabled"> flags;

  flags.set<"visible">();
  flags.set<"enabled">();

  std::cout << std::boolalpha;
  std::cout << "Visible: " << flags.test<"visible">() << "\n";
  std::cout << "Enabled: " << flags.test<"enabled">() << "\n";
}
```

## Additional Utilities

### make_tuple

```c++
#include <Named/NamedTuple.hpp>

namespace named = mguid;

int main() {
  auto color = named::make_tuple(
    named::NamedTypeV<"r">(255),
    named::NamedTypeV<"g">(128),
    named::NamedTypeV<"b">(64)
  );

  int green = color.get<"g">();
}
```

### tuple_cat

```c++
#include <Named/NamedTuple.hpp>
#include <iostream>

namespace named = mguid;

int main() {
  auto a = named::make_tuple(mguid::NamedTypeV<"x">(1));
  auto b = named::make_tuple(mguid::NamedTypeV<"y">(2));

  auto result = named::tuple_cat(a, b);

  std::cout << result.get<"x">() << ", " << result.get<"y">() << "\n";
}
```

### Compile-Time Diagnostics

If you use an invalid tag such as:

```c++
tuple.get<"z">();
```

If you are using GCC or Clang a message will be somewhere in the compiler error telling you the closest key to the one provided:

```
"z" was not found in ["x", "y"]. Did you mean "y"?
```

This is a sort of misuse of the type system.

Unfortunately, there is no way to static assert and show a compile time generated message ***yet***.

## License

This project is licensed under the MIT License.
