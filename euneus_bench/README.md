# EuneusBench

An Elixir project to run benchmarks.

## Directory structure

The scripts are in the `./script` folder and the JSON files are in `./priv/data`.

## Running the scripts

Run the encode benchmark in the command line via `$ mix encode` and decode via `$ mix decode`.

## Summary

### Encode

<!-- TODO -->

### Decode

<!-- TODO -->

## Details

### Encode

#### With input Blockchain

```
Name             ips        average  deviation         median         99th %
jiffy        11.25 K       88.90 μs   ±334.45%       62.17 μs      105.46 μs
Jason         4.76 K      210.20 μs   ±289.11%       90.80 μs     3432.32 μs
euneus        4.60 K      217.52 μs   ±278.27%       94.88 μs     3429.63 μs
thoas         3.87 K      258.41 μs   ±239.40%      130.62 μs     3448.49 μs
jsone         2.55 K      392.00 μs   ±209.44%      163.48 μs     3532.74 μs
Tiny          2.19 K      455.96 μs   ±202.34%      198.05 μs     3688.43 μs
JSX           1.00 K      998.66 μs   ±130.37%      461.71 μs     4264.28 μs

Comparison:
jiffy        11.25 K
Jason         4.76 K - 2.36x slower +121.30 μs
euneus        4.60 K - 2.45x slower +128.62 μs
thoas         3.87 K - 2.91x slower +169.51 μs
jsone         2.55 K - 4.41x slower +303.10 μs
Tiny          2.19 K - 5.13x slower +367.06 μs
JSX           1.00 K - 11.23x slower +909.76 μs

Memory usage statistics:

Name      Memory usage
jiffy          8.03 KB
Jason         78.91 KB - 9.82x memory usage +70.88 KB
euneus        83.10 KB - 10.35x memory usage +75.07 KB
thoas         89.41 KB - 11.13x memory usage +81.38 KB
jsone        179.23 KB - 22.32x memory usage +171.20 KB
Tiny         152.79 KB - 19.02x memory usage +144.76 KB
JSX          404.55 KB - 50.37x memory usage +396.52 KB

**All measurements for memory usage were the same**
```

#### With input Giphy

```
Name             ips        average  deviation         median         99th %
jiffy        1163.43        0.86 ms   ±103.08%        0.65 ms        4.49 ms
Jason         429.55        2.33 ms    ±77.26%        1.08 ms        5.27 ms
euneus        427.53        2.34 ms    ±75.78%        1.10 ms        4.77 ms
thoas         401.66        2.49 ms    ±73.03%        1.22 ms        5.40 ms
Tiny          224.98        4.44 ms    ±42.12%        5.69 ms        6.32 ms
jsone         213.04        4.69 ms    ±33.34%        5.34 ms        8.44 ms
JSX            79.13       12.64 ms     ±2.06%       12.59 ms       13.41 ms

Comparison:
jiffy        1163.43
Jason         429.55 - 2.71x slower +1.47 ms
euneus        427.53 - 2.72x slower +1.48 ms
thoas         401.66 - 2.90x slower +1.63 ms
Tiny          224.98 - 5.17x slower +3.59 ms
jsone         213.04 - 5.46x slower +3.83 ms
JSX            79.13 - 14.70x slower +11.78 ms

Memory usage statistics:

Name      Memory usage
jiffy        116.75 KB
Jason        861.52 KB - 7.38x memory usage +744.77 KB
euneus       870.26 KB - 7.45x memory usage +753.51 KB
thoas        861.35 KB - 7.38x memory usage +744.60 KB
Tiny        1728.32 KB - 14.80x memory usage +1611.57 KB
jsone       2239.20 KB - 19.18x memory usage +2122.45 KB
JSX         4915.75 KB - 42.10x memory usage +4799 KB

**All measurements for memory usage were the same**
```

#### With input GitHub

```
Name             ips        average  deviation         median         99th %
jiffy         3.66 K      273.01 μs   ±181.43%      205.32 μs     3856.05 μs
Jason         1.54 K      648.95 μs   ±158.08%      295.02 μs     3781.68 μs
euneus        1.45 K      687.48 μs   ±153.42%      315.12 μs     3816.72 μs
thoas         1.30 K      767.40 μs   ±138.79%      400.54 μs     3881.39 μs
Tiny          0.85 K     1174.42 μs   ±121.52%      523.43 μs     4413.62 μs
jsone         0.66 K     1512.86 μs    ±96.35%      836.70 μs     4243.65 μs
JSX          0.198 K     5062.48 μs     ±3.47%     5040.56 μs     5686.34 μs

Comparison:
jiffy         3.66 K
Jason         1.54 K - 2.38x slower +375.95 μs
euneus        1.45 K - 2.52x slower +414.48 μs
thoas         1.30 K - 2.81x slower +494.40 μs
Tiny          0.85 K - 4.30x slower +901.41 μs
jsone         0.66 K - 5.54x slower +1239.85 μs
JSX          0.198 K - 18.54x slower +4789.47 μs

Memory usage statistics:

Name      Memory usage
jiffy         42.85 KB
Jason        221.14 KB - 5.16x memory usage +178.29 KB
euneus       228.73 KB - 5.34x memory usage +185.88 KB
thoas        228.15 KB - 5.32x memory usage +185.30 KB
Tiny         450.27 KB - 10.51x memory usage +407.42 KB
jsone        682.82 KB - 15.93x memory usage +639.97 KB
JSX         1408.36 KB - 32.87x memory usage +1365.51 KB

**All measurements for memory usage were the same**
```

#### With input GovTrack

```
Name             ips        average  deviation         median         99th %
jiffy          50.95       19.63 ms    ±12.38%       18.90 ms       32.64 ms
thoas          16.92       59.10 ms    ±41.24%       53.61 ms      125.71 ms
euneus         14.95       66.87 ms    ±45.98%       52.57 ms      112.27 ms
Tiny           13.97       71.60 ms     ±4.22%       71.52 ms       89.10 ms
Jason          13.36       74.86 ms    ±11.75%       77.15 ms       79.36 ms
jsone           8.38      119.38 ms     ±3.79%      119.47 ms      131.61 ms
JSX             3.68      271.98 ms     ±5.12%      268.32 ms      318.84 ms

Comparison:
jiffy          50.95
thoas          16.92 - 3.01x slower +39.48 ms
euneus         14.95 - 3.41x slower +47.24 ms
Tiny           13.97 - 3.65x slower +51.98 ms
Jason          13.36 - 3.81x slower +55.24 ms
jsone           8.38 - 6.08x slower +99.76 ms
JSX             3.68 - 13.86x slower +252.35 ms

Memory usage statistics:

Name      Memory usage
jiffy          3.30 MB
thoas         20.67 MB - 6.26x memory usage +17.36 MB
euneus        20.38 MB - 6.17x memory usage +17.07 MB
Tiny          37.08 MB - 11.22x memory usage +33.77 MB
Jason         18.90 MB - 5.72x memory usage +15.60 MB
jsone         45.34 MB - 13.72x memory usage +42.03 MB
JSX           94.38 MB - 28.56x memory usage +91.07 MB

**All measurements for memory usage were the same**
```

#### With input Issue 90

```
Name             ips        average  deviation         median         99th %
jiffy          36.62       27.31 ms     ±1.68%       27.19 ms       29.98 ms
Jason          27.41       36.48 ms     ±5.67%       37.44 ms       40.76 ms
Tiny           23.83       41.96 ms     ±1.68%       41.78 ms       46.00 ms
euneus         23.10       43.29 ms     ±4.85%       44.18 ms       47.87 ms
thoas          16.97       58.94 ms     ±3.58%       59.88 ms       63.39 ms
jsone          16.66       60.01 ms     ±2.76%       60.86 ms       62.33 ms
JSX             8.89      112.50 ms     ±2.52%      111.44 ms      120.88 ms

Comparison:
jiffy          36.62
Jason          27.41 - 1.34x slower +9.17 ms
Tiny           23.83 - 1.54x slower +14.65 ms
euneus         23.10 - 1.59x slower +15.98 ms
thoas          16.97 - 2.16x slower +31.63 ms
jsone          16.66 - 2.20x slower +32.70 ms
JSX             8.89 - 4.12x slower +85.20 ms

Memory usage statistics:

Name      Memory usage
jiffy        0.0125 MB
Jason          0.81 MB - 65.33x memory usage +0.80 MB
Tiny           1.48 MB - 118.49x memory usage +1.46 MB
euneus         1.07 MB - 85.82x memory usage +1.06 MB
thoas          0.82 MB - 66.05x memory usage +0.81 MB
jsone          2.64 MB - 212.20x memory usage +2.63 MB
JSX            4.56 MB - 366.44x memory usage +4.55 MB

**All measurements for memory usage were the same**
```

#### With input JSON Generator

```
Name             ips        average  deviation         median         99th %
jiffy        1192.81        0.84 ms    ±95.84%        0.67 ms        4.51 ms
euneus        413.79        2.42 ms    ±78.50%        1.08 ms        5.43 ms
thoas         410.33        2.44 ms    ±75.35%        1.22 ms        7.85 ms
Jason         409.92        2.44 ms    ±80.57%        1.06 ms        7.71 ms
jsone         271.62        3.68 ms    ±49.03%        4.82 ms        8.11 ms
Tiny          227.98        4.39 ms    ±44.63%        5.65 ms        6.22 ms
JSX           107.01        9.35 ms    ±30.16%        8.77 ms       13.02 ms

Comparison:
jiffy        1192.81
euneus        413.79 - 2.88x slower +1.58 ms
thoas         410.33 - 2.91x slower +1.60 ms
Jason         409.92 - 2.91x slower +1.60 ms
jsone         271.62 - 4.39x slower +2.84 ms
Tiny          227.98 - 5.23x slower +3.55 ms
JSX           107.01 - 11.15x slower +8.51 ms

Memory usage statistics:

Name      Memory usage
jiffy        110.68 KB
euneus       785.96 KB - 7.10x memory usage +675.28 KB
thoas        769.38 KB - 6.95x memory usage +658.70 KB
Jason        778.17 KB - 7.03x memory usage +667.49 KB
jsone       1711.06 KB - 15.46x memory usage +1600.38 KB
Tiny        1489.41 KB - 13.46x memory usage +1378.73 KB
JSX         3588.78 KB - 32.42x memory usage +3478.10 KB

**All measurements for memory usage were the same**
```

#### With input Pokedex

```
Name             ips        average  deviation         median         99th %
jiffy        1679.30        0.60 ms   ±116.62%        0.45 ms        4.02 ms
euneus        639.49        1.56 ms   ±102.24%        0.69 ms        4.35 ms
Jason         629.64        1.59 ms   ±101.95%        0.70 ms        4.38 ms
thoas         614.14        1.63 ms    ±98.29%        0.76 ms        4.41 ms
jsone         319.98        3.13 ms    ±59.24%        4.37 ms        7.88 ms
Tiny          314.44        3.18 ms    ±59.54%        4.58 ms        5.48 ms
JSX           136.07        7.35 ms    ±43.38%        7.42 ms       13.87 ms

Comparison:
jiffy        1679.30
euneus        639.49 - 2.63x slower +0.97 ms
Jason         629.64 - 2.67x slower +0.99 ms
thoas         614.14 - 2.73x slower +1.03 ms
jsone         319.98 - 5.25x slower +2.53 ms
Tiny          314.44 - 5.34x slower +2.58 ms
JSX           136.07 - 12.34x slower +6.75 ms

Memory usage statistics:

Name      Memory usage
jiffy         51.82 KB
euneus       673.63 KB - 13.00x memory usage +621.80 KB
Jason        658.96 KB - 12.72x memory usage +607.14 KB
thoas        670.49 KB - 12.94x memory usage +618.67 KB
jsone       1490.80 KB - 28.77x memory usage +1438.98 KB
Tiny        1285.62 KB - 24.81x memory usage +1233.80 KB
JSX         3497.85 KB - 67.50x memory usage +3446.03 KB

**All measurements for memory usage were the same**
```

#### With input UTF-8 unescaped

```
Name             ips        average  deviation         median         99th %
jiffy        11.93 K       83.82 μs    ±38.61%       81.90 μs      109.49 μs
Jason        10.01 K       99.89 μs   ±254.98%       80.25 μs      120.11 μs
euneus        9.41 K      106.22 μs   ±245.40%       84.28 μs      133.73 μs
thoas         9.06 K      110.35 μs   ±222.05%       91.50 μs      131.63 μs
jsone         1.57 K      636.31 μs   ±162.79%      261.72 μs     3419.18 μs
JSX           0.53 K     1874.81 μs    ±86.05%      675.30 μs     4154.88 μs
Tiny          0.49 K     2060.35 μs    ±80.56%      835.77 μs     4370.51 μs

Comparison:
jiffy        11.93 K
Jason        10.01 K - 1.19x slower +16.07 μs
euneus        9.41 K - 1.27x slower +22.40 μs
thoas         9.06 K - 1.32x slower +26.53 μs
jsone         1.57 K - 7.59x slower +552.49 μs
JSX           0.53 K - 22.37x slower +1790.99 μs
Tiny          0.49 K - 24.58x slower +1976.53 μs

Memory usage statistics:

Name      Memory usage
jiffy          1.13 KB
Jason          6.02 KB - 5.32x memory usage +4.89 KB
euneus         8.53 KB - 7.53x memory usage +7.40 KB
thoas          5.98 KB - 5.28x memory usage +4.84 KB
jsone        207.50 KB - 183.17x memory usage +206.37 KB
JSX          598.79 KB - 528.59x memory usage +597.66 KB
Tiny         559.43 KB - 493.84x memory usage +558.30 KB

**All measurements for memory usage were the same**
```
