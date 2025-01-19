# Inline Assembly in Frost

Frost provides a powerful inline assembly interface that allows direct hardware
access while maintaining safety and clarity.

## Basic Syntax

The inline assembly block uses a structured format:

```frost
__asm__ {
    code -> "assembly instructions"
    constraints -> "constraint string"
    args -> (variables)
    parameters -> parameter types
    return_type -> return type
    side_effects -> boolean
    align_stack -> boolean
    dialect -> assembly dialect (ATT or Intel)
}
```

## Assembly Block Components

### Code Section

Contains the actual assembly instructions using placeholders:

```frost
code -> "mov $1, $0    % Move second operand to first
        add $0, $0, $2 % Add third operand to first"
```

### Constraints

Specifies how variables map to registers or memory:

```frost
constraints -> "=r,r,r"  % Output in register, inputs in registers
```

**Common Constraints**

- `r`: Register operand
- `m`: Memory operand
- `i`: Immediate integer
- `=`: Write-only operand
- `+`: Read-write operand

### Arguments

Variables passed to assembly:

```frost
args -> (x y z)  % Maps variables to $0, $1, $2
```

### Type Specifications

```frost
parameters -> int int    % Input parameter types
return_type -> int       % Return type
```

## Safety and Optimization

### Side Effects Declaration

```frost
side_effects -> true   % Indicates memory modification
side_effects -> false  % Pure computation only
```

### Stack Alignment

```frost
align_stack -> true    % Ensure proper stack alignment
```

### Assembly Dialects

```frost
dialect -> ATT    % AT&T syntax
dialect -> Intel  % Intel syntax
```

## Best Practices

1. **Minimal Usage**

Prefer high-level Frost when possible. Use assembly only for:

- Hardware access
- Performance-critical sections
- Platform-specific instructions

2. **Documentation**

Document inline assembly blocks for clarity. Add a header with function name and
purpose:

```frost
%%
% Function: sum
% Adds two integers and returns the result.
%%
sum: int int -> int = a b {
    __asm__ {
        code -> "mov $1, $0
        add $0, $0, $2"
        constraints -> "=r,r,r"
        args -> (a b)
        parameters -> int int
        return_type -> int
        side_effects -> false
        align_stack -> false
        dialect -> ATT
    }
}
```

3. **Error Handling**

Always verify assembly execution and handle errors:

```frost
result: bool = critical_asm_operation()

if not result {
    handle_error()
}
```

Remember that inline assembly should be used judiciously, only when necessary
for performance optimization or hardware access. Frost's type system ensures
that inline assembly blocks are safe and properly integrated with the rest of
your code.
