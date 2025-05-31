---
sidebar_position: 5
---

# Stable memory and regions

<!---The small code blocks in this these snippets build upon each other, it might be useful to have a single program run in the backend to eliminate unbound errors-->

<!---This section could also benefit from illustrations but i'm not sure what md will allow-->

Canisters use two types of storage: **Wasm memory** and **stable memory**.

**Wasm memory**, also known as [heap memory](https://internetcomputer.org/docs/building-apps/canister-management/storage#heap-memory), is used automatically for heap-allocated objects. It supports both 32-bit and 64-bit addressing and has a maximum size of 4 GiB. When a canister is upgraded, its Wasm memory is cleared.

**Stable memory** persists across canister upgrades and has a maximum size of 500 GiB. Motoko accesses stable memory via its [stable storage feature](https://internetcomputer.org/docs/building-apps/canister-management/storage#motoko-storage-handling), allowing data to be retained through upgrades. Stable regions further enhance this by enabling more structured and flexible memory management.

After a message executes successfully, all memory changes, both in Wasm and stable memory, are automatically committed. If execution fails, none of the changes are saved.

## What is a `Region`?

A [`Region`](https://internetcomputer.org/docs/motoko/base/Region) is an isolated segment of stable memory that can be independently allocated, expanded, and managed. It acts like a dedicated storage area, keeping its contents separate and inaccessible from other parts of the program.

Each `Region` has a starting address and a current size, and it can grow dynamically. Conceptually, a `Region` resembles an array, where data is accessed using byte offsets similar to indexing. However, unlike standard arrays, memory management within a `Region` is manual. Developers must explicitly track how much space is used and where the current write position is to ensure correct data storage and retrieval.

### Pages

A **page** is the fundamental unit of allocation in stable memory, serving as the building block for memory management. Each page has a fixed size of 64 KiB (`65_536` bytes) and is zero-initialized upon allocation, ensuring a clean state before use. Pages are accessed using [byte offsets](https://en.wikipedia.org/wiki/Offset_(computer_science)) for precise control over memory operations.

### Blocks

A **block** is the fundamental unit the ICP runtime uses to manage stable memory. Each block contains 128 stable memory pages, grouping allocations into larger chunks for efficiency. Although memory can still be accessed and manipulated at the page level, the system allocates memory in block-sized increments, i.e., 128 pages at a time. This strategy helps optimize resource usage, minimize fragmentation, and maintain the flexibility of fine-grained, page-level operations.

### Position and offsets

An **offset** refers to a specific byte position within a [`Region`](https://internetcomputer.org/docs/motoko/base/Region), starting at `0`. It is used to locate and access data within the region's allocated memory space. New positions are typically calculated using expressions like `current_position + bytes_used`, allowing developers to track where to write or read data accurately.

Since stable memory does not manage layout automatically, it is the developer's responsibility to manually track what data resides at each offset to ensure correct memory access and avoid data corruption.

## Using a `Region`

It is the developer’s responsibility to correctly manage and interpret the data stored within a `Region`, which can be error-prone due to the lack of automatic layout and type safety. However, the safety and integrity of Motoko’s native heap-allocated values are always preserved, regardless of the contents of stable [`Region`](https://internetcomputer.org/docs/motoko/base/Region) memory.

It’s also important to note that accessing stable regions incurs significantly higher performance costs compared to using Motoko’s native memory.

### Creation and allocation

A new [`Region`](https://internetcomputer.org/docs/motoko/base/Region) is created using `Region.new()`, initializing it with a size of 0 pages. The current size can be checked with `Region.size(myRegion)`, which returns the number of allocated pages. To expand the [`Region`](https://internetcomputer.org/docs/motoko/base/Region), use `Region.grow(myRegion, X)`, where `X` is the number of pages to be added. The function will return the previous size before expansion, allowing verification of successful growth. If the operation fails due to memory constraints, it will return `0xFFFF_FFFF_FFFF_FFFF`, indicating that no additional memory could be allocated.

```motoko no-repl
// Create a new region of initial size 0
let myRegion = Region.new();

// Check the current size (in pages)
let currentSizeInPages = Region.size(myRegion);  // Initially 0

// Grow the region by adding pages
// Returns previous size on success, returns 0xFFFF_FFFF_FFFF_FFFF on failure
let previousSize = Region.grow(myRegion, 10);  // Add 10 pages (640 KiB)
```

### Growing a `Region` safely

`Regions` can only grow; they cannot be shrunk once expanded. Growth may fail due to [ICP resource limitations](https://internetcomputer.org/docs/building-apps/canister-management/resource-limits), therefore it's recommended to allocate only the minimum number of pages necessary to conserve resources.

To grow a [`Region`](https://internetcomputer.org/docs/motoko/base/Region) safely, first check its current size and calculate the additional space needed. Since stable memory expands in fixed 64 KiB pages, compute the required increase by subtracting the current capacity from the desired size, then round up to the nearest page boundary. After allocating the additional pages, always verify that the growth was successful to ensure memory stability and avoid unintended access errors.

```motoko no-repl
 // Helper function to ensure a region has enough space
func ensureCapacity(r : Region, requiredBytes : Nat64) {
  let currentPages = Region.size(r);
  let bytesPerPage : Nat64 = 65536;
  let currentCapacity = currentPages * bytesPerPage;
  
  if (requiredBytes > currentCapacity) {
    // Calculate how many new pages we need
    let additionalBytesNeeded = requiredBytes - currentCapacity;
    let pagesNeeded = (additionalBytesNeeded + bytesPerPage - 1) / bytesPerPage;
    
    // Grow the region and check the result
    let result = Region.grow(r, pagesNeeded);
    assert result == currentPages; // Verify growth succeeded
  };
}
```

### Reading and writing data

Since stable memory does not inherently manage data structure layouts, offsets must be tracked manually to ensure correct placement and retrieval of values.

Smaller data types, such as an 8-bit naturals ([`Nat8`](https://internetcomputer.org/docs/motoko/base/Nat8)), can be written and retrieved from a designated offset. The chosen offset determines where in the [`Region`](https://internetcomputer.org/docs/motoko/base/Region) the value is stored.

```motoko no-repl
// Store an 8-bit value at offset 0
Region.storeNat8(myRegion, 0, 42);

// Read the 8-bit value from offset 0
let value = Region.loadNat8(myRegion, 0);  // Returns 42
```

Larger values, such as [`Nat64`](https://internetcomputer.org/docs/motoko/base/Nat64), require multiple bytes for storage. Allocating offsets with enough space prevents overlapping data.

In this example, the 64-bit integer is stored at offset `100` to ensure it has sufficient space.

```motoko no-repl
// Store a 64-bit value at offset 100
Region.storeNat64(myRegion, 100, 123456789);

// Read the 64-bit value from offset 100
let longValue = Region.loadNat64(myRegion, 100);  // Returns 123456789
```

Floating-point numbers can also be stored in a [`Region`](https://internetcomputer.org/docs/motoko/base/Region). Since these values occupy multiple bytes, offsets should be spaced accordingly to avoid overwriting adjacent data.

```motoko no-repl
// Store a floating-point value at offset 200
Region.storeFloat(myRegion, 200, 3.14159);

// Read the floating-point value from offset 200
let pi = Region.loadFloat(myRegion, 200);  // Returns 3.14159
```

Regions can be used for storing and retrieving binary data as `Blob`s. This is useful for handling arbitrary sequences of bytes, such as serialized objects or encoded information.

`Blob`s can be stored at any offset, but their size must be considered when choosing a starting position. Retrieving the correct number of bytes ensures that data integrity is maintained when working with binary storage in a [`Region`](https://internetcomputer.org/docs/motoko/base/Region).

```motoko no-repl
// Create a blob
let myData = Blob.fromArray([1, 2, 3, 4, 5]);

// Store the blob at offset 300
Region.storeBlob(myRegion, 300, myData);

// Read 5 bytes from offset 300
let retrievedData = Region.loadBlob(myRegion, 300, 5);  // Returns the same blob
```

### Mops packages for Regions

- [`memory-region`](https://mops.one/memory-region]): A library for abstraction over the [`Region`](https://internetcomputer.org/docs/motoko/base/Region) type that supports reusing deallocated memory.

- [`stable-enum`](https://mops.one/stable-enum): Enumerations implemented in stable regions.

- [`stable-buffer`](https://mops.one/stable-buffer): Buffers implemented in stable regions.

## Memory layout strategies for using regions

While the basic operations discussed above show how to store and retrieve individual values in a [`Region`](https://internetcomputer.org/docs/motoko/base/Region), real-world applications typically demand more structured memory management. To address this, various memory layout strategies build on these foundational operations, enabling more organized and scalable data storage within regions.

These strategies offer systematic approaches for allocating, tracking, and accessing data, making it possible to implement complex data structures efficiently. Each approach presents its own balance of simplicity, flexibility, and performance, allowing developers to choose the one that best fits their application's specific requirements.

### Sequential

In sequential storage, data is written one item after another, with each new entry appended to the end of the [`Region`](https://internetcomputer.org/docs/motoko/base/Region). To avoid overwriting existing data, you must manually track the next available offset. A common pattern is to store the size of each data item just before its actual content. This allows for efficient traversal and retrieval by reading the size first, then skipping ahead accordingly.

```motoko no-repl
var nextPosition : Nat64 = 0;

// Store a series of values sequentially
func storeSequential(value : Blob) : Nat64 {
  let position = nextPosition;
  let size = Nat64.fromNat(value.size());
  
  // Store the size followed by the data
  Region.storeNat64(myRegion, position, size);
  Region.storeBlob(myRegion, position + 8, value);
  
  // Update the next available position
  nextPosition := position + 8 + size;
  
  // Return the position where we stored the data
  position
}
```

### Fixed-size entry

In fixed-size storage, memory is divided into uniformly sized slots, allowing each entry to be accessed at a predictable offset using the formula `index * ENTRY_SIZE`. This approach simplifies both indexing and retrieval, as the location of any entry can be calculated directly. However, it can lead to wasted space if the stored data is smaller than the fixed slot size, making it less efficient in scenarios with highly variable data sizes.

```motoko no-repl
let ENTRY_SIZE : Nat64 = 100;  // Each entry is 100 bytes

// Store in a specific entry slot
func storeEntry(index : Nat64, data : Blob) {
  assert Nat64.fromNat(data.size()) <= ENTRY_SIZE;
  
  let position = index * ENTRY_SIZE;
  Region.storeBlob(myRegion, position, data);
}

// Read from a specific entry slot
func readEntry(index : Nat64, size : Nat) : Blob {
  let position = index * ENTRY_SIZE;
  Region.loadBlob(myRegion, position, size)
}
```

### Index-based

For variable-sized data, an index [`Region`](https://internetcomputer.org/docs/motoko/base/Region) is used to track where each entry is stored within a separate data `Region`. This method maintains two separate memory regions: one for **indexes** that store positions and sizes and one for **actual data**. Using an index allows efficient access to variable-length data while avoiding fragmentation issues.

```motoko no-repl
// Two regions: one for index, one for data
let indexRegion = Region.new();
let dataRegion = Region.new();
var dataPosition : Nat64 = 0;

// Store data and update index
func storeWithIndex(index : Nat64, data : Blob) {
  let size = Nat64.fromNat(data.size());
  
  // Store position and size in the index
  Region.storeNat64(indexRegion, index * 16, dataPosition);
  Region.storeNat64(indexRegion, index * 16 + 8, size);
  
  // Store actual data
  Region.storeBlob(dataRegion, dataPosition, data);
  
  // Update next data position
  dataPosition += size;
}

// Retrieve data using index
func retrieveByIndex(index : Nat64) : Blob {
  let position = Region.loadNat64(indexRegion, index * 16);
  let size = Region.loadNat64(indexRegion, index * 16 + 8);
  
  Region.loadBlob(dataRegion, position, Nat64.toNat(size))
}
```

## Best practices

Managing stable memory effectively requires structuring data in a way that ensures compatibility, scalability, and reliability. It is best practice to use versioning, metadata headers, and transaction logs to help maintain a well-organized memory layout.

### Versioning data

Including a version marker at the beginning of a [`Region`](https://internetcomputer.org/docs/motoko/base/Region) ensures compatibility across different data formats and future upgrades. This approach ensures that older versions of the stored data remain accessible while allowing new formats to be introduced. The version marker, typically stored at offset `0`, is checked before performing any operations.

```motoko no-repl
// When initializing a region
if (Region.size(myRegion) == 0) {
  // Initialize with version 1
  Region.grow(myRegion, 1);
  Region.storeNat64(myRegion, 0, 1);
}

// When reading, check version
let version = Region.loadNat64(myRegion, 0);
if (version == 1) {
  // Handle version 1 data format
} else if (version == 2) {
  // Handle version 2 data format
}
```

### Metadata header

A metadata header at the beginning of a [`Region`](https://internetcomputer.org/docs/motoko/base/Region) allows efficient tracking of data version, entry count, and available space. This structure provides a centralized reference for managing stored data, preventing unnecessary scanning of the entire `Region` to determine its state.

```motoko no-repl
// Metadata structure:
// Offset 0: Version (8 bytes)
// Offset 8: Entry count (8 bytes)
// Offset 16: Next available position (8 bytes)
// Offset 24: Data starts here

// Update metadata
func updateMetadata(version : Nat64, count : Nat64, position : Nat64) {
  Region.storeNat64(myRegion, 0, version);
  Region.storeNat64(myRegion, 8, count);
  Region.storeNat64(myRegion, 16, position);
}

// Read metadata
func getMetadata() : {version : Nat64; count : Nat64; position : Nat64} {
  {
    version = Region.loadNat64(myRegion, 0);
    count = Region.loadNat64(myRegion, 8);
    position = Region.loadNat64(myRegion, 16);
  }
}
```

### Transaction log pattern

The transaction log pattern enables efficient sequential appends while preserving a structured and reliable state. New log entries are added in order, ensuring data integrity and minimizing the risk of corruption. This approach is particularly well-suited for event-driven architectures, where each change is recorded as an immutable entry.


```motoko no-repl
// Add an entry to a log
func appendToLog(data : Blob) {
  let meta = getMetadata();
  let position = meta.position;
  let size = Nat64.fromNat(data.size());
  
  // Store size followed by data
  Region.storeNat64(myRegion, position, size);
  Region.storeBlob(myRegion, position + 8, data);
  
  // Update metadata
  updateMetadata(
    meta.version, 
    meta.count + 1, 
    position + 8 + size
  );
}
```

## `Regions` comprehensive example

This example illustrates the simultaneous use of stable variables and stable memory. It uses a single stable variable, `state`, to keep track of the two regions and their size in bytes, but stores the contents of the log directly in stable memory.

```motoko no-repl
import Nat64 "mo:base/Nat64";
import Region "mo:base/Region";

persistent actor StableLog {

  // Index of saved log entry.
  public type Index = Nat64;

  // Internal representation uses two regions, working together.
  var state = { // implicitly `stable`
    bytes = Region.new();
    var bytes_count : Nat64 = 0;
    elems = Region.new ();
    var elems_count : Nat64 = 0;
  };

  // Grow a region to hold a certain number of total bytes.
  func regionEnsureSizeBytes(r : Region, new_byte_count : Nat64) {
    let pages = Region.size(r);
    if (new_byte_count > pages << 16) {
      let new_pages = ((new_byte_count + ((1 << 16) - 1)) / (1 << 16)) - pages;
      assert Region.grow(r, new_pages) == pages
    }
  };

  // Element = Position and size of a saved a Blob.
  type Elem = {
    pos : Nat64;
    size : Nat64;
  };

  transient let elem_size = 16 : Nat64; /* two Nat64s, for pos and size. */

  // Count of elements (Blobs) that have been logged.
  public func size() : async Nat64 {
      state.elems_count
  };

  // Constant-time random access to previously-logged Blob.
  public func get(index : Index) : async Blob {
    assert index < state.elems_count;
    let pos = Region.loadNat64(state.elems, index * elem_size);
    let size = Region.loadNat64(state.elems, index * elem_size + 8);
    let elem = { pos ; size };
    Region.loadBlob(state.bytes, elem.pos, Nat64.toNat(elem.size))
  };

  // Add Blob to the log, and return the index of it.
  public func add(blob : Blob) : async Index {
    let elem_i = state.elems_count;
    state.elems_count += 1;

    let elem_pos = state.bytes_count;
    state.bytes_count += Nat64.fromNat(blob.size());

    regionEnsureSizeBytes(state.bytes, state.bytes_count);
    Region.storeBlob(state.bytes, elem_pos, blob);

    regionEnsureSizeBytes(state.elems, state.elems_count * elem_size);
    Region.storeNat64(state.elems, elem_i * elem_size + 0, elem_pos);
    Region.storeNat64(state.elems, elem_i * elem_size + 8, Nat64.fromNat(blob.size()));
    elem_i
  }
};
```

## Performance considerations

Efficient use of stable memory involves optimizing read and write operations to minimize overhead. Grouping multiple small writes into fewer, larger batches improves performance by reducing the number of memory transactions. Aligning large data structures to page boundaries is also beneficial, as crossing page boundaries can increase processing costs. Frequently accessed data should be cached in native memory to avoid the slower reads from stable memory. Since stable memory is persistent, temporary or short-lived data is better kept in native memory to reduce unnecessary stable memory usage.

### Memory usage estimation

To estimate the memory needed, calculate the expected storage based on the average entry size and the total number of entries. Multiply the average entry size by the expected number of entries, then add an overhead buffer (usually around 10–20%) to account for metadata and alignment. Finally, divide this total size by 65_536 bytes (the size of one stable memory page) to determine how many pages must be allocated.

For example, if each entry is 200 bytes and the application expects to store 1_000 entries, the total raw memory usage is:

$$
1000 \times 200 = 200,000 \text{ bytes}
$$

Adding a 20% overhead increases this to:

$$
200,000 \times 1.2 = 240,000 \text{ bytes}
$$

To determine the number of stable memory pages needed, divide by the page size (65_536 bytes):

$$
\frac{240,000}{65,536} \approx 3.66
$$

Since stable memory allocations must be whole pages, this rounds up to 4 pages.

## Troubleshooting

Stable memory management can introduce issues such as out-of-bounds access, growth failures, and data corruption.

One common error is `accessing memory beyond allocated limits`, indicating out-of-bounds access. This occurs when attempting to read or write data beyond the [`Region`](https://internetcomputer.org/docs/motoko/base/Region)’s current size. To prevent this, always ensure the `Region` has been expanded sufficiently before performing operations.

A `Region` may also fail to grow when calling `Region.grow()`, returning `0xFFFF_FFFF_FFFF_FFFF` instead of the previous size. This typically indicates that the stable memory limit has been reached. Implementing fallback strategies or optimizing memory usage can help mitigate this issue.

Data corruption can occur when memory layouts are mismanaged, resulting in unintended overwrites. Proper bounds checking and structured memory layouts help prevent this problem, ensuring that stored data remains intact.

### Debugging

Tracking memory usage is crucial to prevent overflow and inefficiencies. Maintaining counters for allocated versus used memory allows early detection of excessive usage before it causes failures. Adding assertions at critical points in the code helps verify that offset calculations stay within valid bounds.

To ensure data integrity, implementing checksums can detect corruption by verifying whether stored data has been altered unexpectedly. Additionally, placing known markers at structure boundaries aids in identifying memory misalignment and assists with debugging.