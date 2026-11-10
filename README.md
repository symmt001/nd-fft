# nd-fft: Multi-Dimensional Radix-2 FFT

[![DOI](https://img.shields.io/badge/DOI-10.1016%2FS0010--4655(99)00456--7-blue.svg)](https://doi.org/10.1016/S0010-4655(99)00456-7)
[![Language: Fortran](https://img.shields.io/badge/language-Fortran%20%2F%20C-orange.svg)](#)

This repository is a static archive preserved for reference. It contains the implementation code and benchmark framework accompanying the academic paper published in *Computer Physics Communications* (2000). The source code is made publicly available for readers of the paper.

---

## 📄 Reference & Paper Information

If you refer to or use this codebase, please cite the original publication:

* **Title**: Effective implementations of multi-dimensional radix-2 FFT
* **Author**: Susumu Yamamoto
* **Journal**: *Computer Physics Communications*, Volume 125, Issues 1–3, Pages 1–7 (March 2000)
* **DOI**: [10.1016/S0010-4655(99)00456-7](https://doi.org/10.1016/S0010-4655(99)00456-7)

### BibTeX
```bibtex
@article{YAMAMOTO20001,
  title   = {Effective implementations of multi-dimensional radix-2 FFT},
  author  = {Susumu Yamamoto},
  journal = {Computer Physics Communications},
  volume  = {125},
  number  = {1},
  pages   = {1--7},
  year    = {2000},
  issn    = {0010-4655},
  doi     = {10.1016/S0010-4655(99)00456-7},
  url     = {https://doi.org/10.1016/S0010-4655(99)00456-7}
}
```

### Abstract (Summary)
The Discrete Fourier Transform (DFT) can theoretically be executed in $\mathcal{O}(N \log N)$ time using the Fast Fourier Transform (FFT). However, in multi-dimensional transforms where data sizes scale rapidly, an implementation that does not take the underlying processor architecture and memory hierarchy into account can degrade performance by more than an order of magnitude. This research explores efficient multi-dimensional radix-2 FFT algorithms tailored to **RISC workstations** (focusing on cache utilization and memory blocking) and **vector-type supercomputers** (maximizing vector length and avoiding stride bank conflicts).

---

## 🚀 Overview of Implementations (`nd1` - `nd25`)

The repository contains progressive implementations (`nd1.f` through `nd25.f`) exploring different algorithmic and architectural strategies:

| Implementation | Description & Optimization Techniques |
| :--- | :--- |
| **`nd1`** | Baseline decimation implementation (labeled DIT in legacy comments; DIF/Gentleman–Sande formulation). |
| **`nd2`** | Loop merging at the bit-reversal stage: maximizes the length of the innermost loop. |
| **`nd3`** | Real-data version of `nd2`. |
| **`nd4`** | Multiple 1D FFT calls with a small workspace. |
| **`nd5`** | Recursive call strategy with specialized initial decimation passes (similar to `nd25`). |
| **`nd6`** | `nd2` with special treatment in the first decimation stage. |
| **`nd7`** | Recursive call strategy with specialized final stages. |
| **`nd8`** | Multiple FFT calls combined with matrix transposition routines. |
| **`nd9`** | `nd8` enhanced with a self-sorting algorithm (eliminating explicit bit-reversal). |
| **`nd10`** | `nd7` combined with strided multiple FFT (`mfftnbs`). |
| **`nd11`** | Multiple FFT calls with BLAS-based transpose routines. |
| **`nd12`** | Multiple FFT calls without bit-reversal. |
| **`nd15`** | Small workspace blocking + multiple FFT without bit-reversal (effective cache-line utilization). |
| **`nd16`** | `nd15` using transposition instead of workspace buffer; designed to avoid memory bank conflicts. |
| **`nd18`** | Prototype of `nd20`: multiple FFT with bit-reversal integrated into compute loops. |
| **`nd20`** | Multiple FFT with minimal memory copy, integrated bit-reversal, specialized 8/4/2-point kernels. |
| **`nd20_asm`** | `nd20` linked with SSE2 streaming-store assembly transpose kernel (`transp_1.o`). |
| **`nd20x`** | Variant of `nd20` with separated contiguous real and imaginary arrays (yielded +30% speedup on Fujitsu VPP). |
| **`nd21`** | Simplified `nd16` with explicit transpose (effective on machines with fast memory copy). |
| **`nd22`** | Reduced outer loop counts for the 1st dimension while expanding innermost loops (well-balanced for scalar and vector). |
| **`nd24`** | `nd15` + specialized 16/8/4/2-point unrolled butterfly kernels. |
| **`nd25`** | Refined memory workspace management on top of `nd24`. |

### Comparative Benchmarks
The codebase also includes interfaces to compare performance against other established FFT packages of that era:
* `ndfftw`: FFTW library (`fftwnd`)
* `ndasl`: NEC ASL library (`JFCSSF`)
* `ndmfft`: `C3FFT` from `mfft`
* `ndgpfa` / `ndgpfa2`: Clive Temperton's Generalized Prime Factor FFT (GPFA) with vector optimization directives

---

## 📝 Note on Terminology: DIT vs. DIF

### Mislabeling in Source Comments
Throughout the Fortran source files (`fftnd_*.f`, `mfft_*.f`, `dec*.f`), the routines are repeatedly labeled in the comments as `Decimation-In-Time Method`.

This labeling is an error caused by the author's insufficient knowledge and lack of understanding of FFT algorithmic taxonomy at the time of development (late 1990s):
* **Actual Implementation**: The butterfly kernels (e.g., in `decb3fm_1.f`, `mfftw_2.f`) calculate additions and subtractions across strides *before* multiplying the twiddle factors, and the execution stages progress top-down from the largest stride ($N$) down to the smallest stride ($2$).
* **Mathematical Classification**: This structure corresponds to **DIF (Decimation-In-Frequency)** / the **Gentleman–Sande (GS)** butterfly formulation, rather than DIT.
* **Preservation**: The original comments in the `.f` files remain unedited solely to preserve the legacy codebase as it was originally written.

### Note on "Without Bit-Reversal" (`mfftnb`, `nd5`, `nd12`, `nd21`)
Routines designated with `nb` (such as `mfftnb_*.f`) execute *"without bit-reversal"*, leaving the data in bit-reversed order.
In bidirectional (round-trip) FFT workflows, omitting bit-reversal may potentially reduce data rearrangement and computational overhead.

---

## 💻 Historical Architecture Benchmarks

The implementations were benchmarked across major supercomputers and workstations:
* **NEC SX-3 & SX-4** (Vector supercomputers): Vector pipeline optimization and loop merging
* **Hitachi S-3800** (Vector supercomputer): Large memory bandwidth and hardware list-vector support
* **Fujitsu VPP500** (Parallel vector architecture): Memory arrangement exploration (`nd20x`)
* **HP J2240 (PA-RISC)**: Cache memory hierarchy and blocking optimizations
* **MPI Cluster**: Parallel multi-dimensional FFT implementation (`fftnd_mpi_*.f`)

---

## 🛠️ Usage Example

The basic workflow consists of preparation routines followed by the execution routine:

```fortran
      program sample
      implicit none
      integer, parameter :: ND = 3
      integer :: n(ND)
      complex*16, allocatable :: x(:,:,:), cwk(:,:,:), phase(:,:)
      integer, allocatable :: bitrev(:,:)
      integer :: NM, NTOT

      ! Dimensions: 2^n(1) x 2^n(2) x 2^n(3)
      n = (/ 6, 6, 6 /)  ! 64 x 64 x 64
      NM = 2**maxval(n)

      allocate(x(0:2**n(1)-1, 0:2**n(2)-1, 0:2**n(3)-1))
      allocate(cwk(2**n(1), 2**n(2), 2**n(3)))
      allocate(phase(0:NM/2-1, ND))
      allocate(bitrev(NM, ND))

      ! 1. Generate bit-reversal and phase twiddle-factor tables
      call fftprep1(ND, NM, n, bitrev)
      call fftprep2(ND, NM, n, phase)
      call fftprep4(ND, NM, n, phase, bitrev)

      ! Initialize array x ...

      ! 2. Forward FFT
      call fftnd(ND, NM, n, x, cwk, phase, bitrev)

      ! 3. Inverse FFT (IDFT)
      call fftprep3(ND, NM, phase)  ! Conjugate phase factors
      call fftnd(ND, NM, n, x, cwk, phase, bitrev)
      call fftipost(ND, n, x)       ! Normalize by 1/N
      end
```

See [`MulDimFFT.sample_main.f`](MulDimFFT.sample_main.f) for the complete benchmark and verification routine.

---

## 🔨 Build & Quick Start

Requirements:
* **GNU Fortran (`gfortran`)** and **C Compiler (`gcc`)**
* **BLAS library** (e.g., OpenBLAS or reference BLAS for `nd11`)

The default `Makefile` is configured out-of-the-box for `gfortran` & `gcc` (`-O3 -std=legacy`).

```bash
# 1. Select a test problem size / iteration setting (test0 to test4)
make test0     # Sets 128x128x128, 5 iterations

# 2. Build a specific FFT routine (e.g., nd2, nd20, or nd25)
make nd2
./nd2

# Build assembly-accelerated version (transp_1.o)
make nd20_asm
./nd20_asm

# Build BLAS-transposed version (transp_blas.f)
make nd20_blas
./nd20_blas

# 3. Or build all available implementations
make all

# 4. Clean up executables and object files
make clean
```

---

## 📁 Repository Structure

* `nd1.f` - `nd25.f`: Core N-dimensional FFT routines.
* `fftprep*.f`: Precalculation routines (bit-reversal tables, twiddle factors).
* `transp*.f`, `transb*.f`, `rearr*.f`: Transpose and memory rearrangement kernels.
* `fftnd_mpi_*.f`: MPI-distributed multidimensional FFT routines.
* `MulDimFFT.sample_main.f`, `bench*.f`, `ffttest*.f`: Verification and benchmark testbeds.
* `description.txt`: Original author's documentation of algorithm variants and compiler flags.
* `para.txt`: Personal benchmark and configuration notes on Hitachi SR8000.

---

## ⚡ Appendix: Modern 64-bit Port & Assembly Optimization

### Background & 64-bit Port
In 2002, the author manually optimized the 3D-FFT matrix transpose routine by converting compiler-generated assembly of `transp_2.f` to use **SSE2 non-temporal streaming stores (`movntpd`)**, preventing massive cache spills during large array transpositions.

Because the original `transp1.s` was written for 32-bit IA-32 (Pentium 4) conventions, in November 2026, **Antigravity CLI (`agy`)**, working under the direction of **Susumu Yamamoto**, adapted this non-temporal blocking policy into a pure 64-bit x86_64 System V ABI assembly implementation: [`transp1_x86_64.s`](transp1_x86_64.s). New Makefile targets (`nd20_asm` and `nd8_asm`) link this assembly kernel (`transp_1.o`).

### Modern Benchmark Results (3D-FFT $128 \times 128 \times 128$)
* **Size**: $128 \times 128 \times 128$ ($2,097,152$ complex numbers $\approx 33.55\text{ MB}$), $M=21$ transforms, averaged over 3 independent runs. All runs passed `MaxError < EPS` ($1.0 \times 10^{-12}$).
* **Test Environment**: AMD EPYC 9135 16-Core Processor (Zen 5, 16C/32T, up to 4.3 GHz), 256 GB DDR5 ECC, Ubuntu 24.04 LTS (x86_64), `gfortran 13.3.0 -O3 -std=legacy`, OpenBLAS 0.3.26.

| Implementation | Transpose Kernel | Measured TIME/M (ms) | Averaged TIME/M | Averaged MFLOPS | Speedup vs. BLAS | Speedup vs. Fortran |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **`nd20_asm`** (ASM) | `transp1_x86_64.s` (SSE2 `movntpd`) | 29.0, 29.5, 28.6 | **29.0 ms** | **7,580 MFLOPS** | **1.95x** | **1.40x (+40%)** |
| **`nd20`** (Fortran) | `transp_1.f` ($32 \times 32$ Blocking) | 40.5, 40.5, 41.4 | 40.8 ms | 5,400 MFLOPS | 1.39x | 1.00x (Baseline) |
| **`nd20_blas`** (BLAS) | `transp_blas.f` (OpenBLAS `zcopy`) | 56.7, 56.7, 56.2 | 56.5 ms | 3,900 MFLOPS | 1.00x (Baseline) | 0.72x |
| **`nd8_asm`** (ASM) | `transp1_x86_64.s` (SSE2 `movntpd`) | 35.2, 36.2, 35.7 | **35.7 ms** | **6,170 MFLOPS** | **1.68x** | **1.16x (+16%)** |
| **`nd8`** (Fortran) | `transp_1.f` ($32 \times 32$ Blocking) | 41.0, 42.4, 40.5 | 41.3 ms | 5,340 MFLOPS | 1.45x | 1.00x (Baseline) |
| **`nd8_blas`** (BLAS) | `transp_blas.f` (OpenBLAS `zcopy`) | 60.0, 60.0, 60.0 | 60.0 ms | 3,670 MFLOPS | 1.00x (Baseline) | 0.69x |

#### Isolated Matrix Transpose Kernel (32 MB Array, $P=128, Q=16384$)
* **SSE2 Assembly** ([`transp1_x86_64.s`](transp1_x86_64.s)): **2.40 ms** (~26.7 GB/s) — **4.7x faster than BLAS**
* **Fortran Cache-Blocking** ([`transp_1.f`](transp_1.f)): **5.10 ms** (~12.5 GB/s) — **2.2x faster than BLAS**
* **OpenBLAS Strided** ([`transp_blas.f`](transp_blas.f) / `zcopy`): **11.30 ms** (~5.7 GB/s)

> **Architectural Takeaway**: Standard BLAS 1D routines (`zcopy`) lack 2D spatial cache blocking and suffer from power-of-two stride cache/TLB thrashing. Tailored cache-line tiling ([`transp_1.f`](transp_1.f)) and non-temporal streaming stores ([`transp1_x86_64.s`](transp1_x86_64.s)) substantially outperform general-purpose vector library routines for multi-dimensional array transposition.

---

## 👤 Author
* **山元 進 (Susumu Yamamoto)**
  * Affiliation: Tokyo University of Technology (東京工科大学)
  * Affiliation at publication: Department of Applied Physics, The University of Tokyo (東京大学 大学院工学系研究科 物理工学専攻)
  * GitHub: [@symmt001](https://github.com/symmt001)

---

## 🤝 AI Pair Programming & Modernization
* **Antigravity CLI (`agy`)** (Google DeepMind):
  * Modernized the legacy codebase to compile and run out-of-the-box on modern 64-bit Linux (`gfortran` / `gcc`).
  * Created the 64-bit x86_64 assembly transpose implementation ([`transp1_x86_64.s`](transp1_x86_64.s)) adapting the author's original 32-bit Pentium 4 non-temporal streaming-store optimization under the direction of Susumu Yamamoto.
  * Conducted verification benchmarks and assisted in documentation preservation.
