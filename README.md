# 🐫⚙️ OCaml Functional Programming Utils

## 📝 Description
This project implements a collection of functional programming utilities in OCaml, featuring the Collatz conjecture and various list manipulation functions. It was developed as part of the Functional and Logic Programming course.  

### ✨ Key Features
- **Collatz Sequence Implementation**: 
  - `collatz`: Applies the Collatz sequence for a specified number of iterations
  - `collatz3`: Recursively applies the Collatz sequence until reaching 1, 2, or 4

- ** List Manipulation Functions**:
  - `between`: Generates a list of integers within a specified range
  - `count`: Counts occurrences of elements in a list
  - `map`: Custom implementation of the map function
  - `divide`: Calculates proportions of elements in classified lists

## ▶️ Usage
The module provides essential functional programming tools that can be used for:
- Number sequence analysis
- List processing and transformation
- Statistical calculations
- Custom data mapping and filtering

This collection serves as both an educational resource for understanding functional programming concepts and a practical toolkit for OCaml development.

## Installation

### ✅ Prerequisites
- 🐫 OCaml (version 4.x or higher)
- 📦 OPAM (OCaml Package Manager)

### Setup
1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/ocaml-functional-utils.git
   cd ocaml-functional-utils
   ```

2. Compile the code:
   ```bash
   ocamlc -o program utils.ml
   ```
