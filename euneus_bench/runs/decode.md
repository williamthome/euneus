Benchmark

Benchmark run from 2023-11-17 20:09:21.621107Z UTC

## System

Benchmark suite executing on the following system:

<table style="width: 1%">
  <tr>
    <th style="width: 1%; white-space: nowrap">Operating System</th>
    <td>Linux</td>
  </tr><tr>
    <th style="white-space: nowrap">CPU Information</th>
    <td style="white-space: nowrap">Intel(R) Core(TM) i7-7700HQ CPU @ 2.80GHz</td>
  </tr><tr>
    <th style="white-space: nowrap">Number of Available Cores</th>
    <td style="white-space: nowrap">8</td>
  </tr><tr>
    <th style="white-space: nowrap">Available Memory</th>
    <td style="white-space: nowrap">15.54 GB</td>
  </tr><tr>
    <th style="white-space: nowrap">Elixir Version</th>
    <td style="white-space: nowrap">1.16.0-dev</td>
  </tr><tr>
    <th style="white-space: nowrap">Erlang Version</th>
    <td style="white-space: nowrap">26.1</td>
  </tr>
</table>

## Configuration

Benchmark suite executing with the following configuration:

<table style="width: 1%">
  <tr>
    <th style="width: 1%">:time</th>
    <td style="white-space: nowrap">10 s</td>
  </tr><tr>
    <th>:parallel</th>
    <td style="white-space: nowrap">1</td>
  </tr><tr>
    <th>:warmup</th>
    <td style="white-space: nowrap">1 s</td>
  </tr>
</table>

## Statistics



__Input: Blockchain__

Run Time

<table style="width: 1%">
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Average</th>
    <th style="text-align: right">Devitation</th>
    <th style="text-align: right">Median</th>
    <th style="text-align: right">99th&nbsp;%</th>
  </tr>

  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap; text-align: right">5.94 K</td>
    <td style="white-space: nowrap; text-align: right">168.29 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;46.44%</td>
    <td style="white-space: nowrap; text-align: right">149.06 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">568.75 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">5.77 K</td>
    <td style="white-space: nowrap; text-align: right">173.46 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;42.67%</td>
    <td style="white-space: nowrap; text-align: right">152.44 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">507.52 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">5.53 K</td>
    <td style="white-space: nowrap; text-align: right">180.94 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;44.52%</td>
    <td style="white-space: nowrap; text-align: right">155.17 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">505.43 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">4.87 K</td>
    <td style="white-space: nowrap; text-align: right">205.28 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;30.66%</td>
    <td style="white-space: nowrap; text-align: right">186.29 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">492.06 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">4.26 K</td>
    <td style="white-space: nowrap; text-align: right">234.74 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;31.94%</td>
    <td style="white-space: nowrap; text-align: right">222.47 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">492.74 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.83 K</td>
    <td style="white-space: nowrap; text-align: right">546.13 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;12.04%</td>
    <td style="white-space: nowrap; text-align: right">549.61 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">706.59 &micro;s</td>
  </tr>

</table>


Run Time Comparison

<table style="width: 1%">
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">5.94 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">5.77 K</td>
    <td style="white-space: nowrap; text-align: right">1.03x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">5.53 K</td>
    <td style="white-space: nowrap; text-align: right">1.08x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">4.87 K</td>
    <td style="white-space: nowrap; text-align: right">1.22x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">4.26 K</td>
    <td style="white-space: nowrap; text-align: right">1.39x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.83 K</td>
    <td style="white-space: nowrap; text-align: right">3.25x</td>
  </tr>

</table>



Memory Usage

<table style="width: 1%">
  <tr>
    <th>Name</th>
    <th style="text-align: right">Average</th>
    <th style="text-align: right">Factor</th>
  </tr>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap">1.55 KB</td>
    <td>&nbsp;</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">51.41 KB</td>
    <td>33.24x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap">51.63 KB</td>
    <td>33.37x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap">51.41 KB</td>
    <td>33.24x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap">151.39 KB</td>
    <td>97.87x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap">311.43 KB</td>
    <td>201.33x</td>
  </tr>
</table>



__Input: Giphy__

Run Time

<table style="width: 1%">
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Average</th>
    <th style="text-align: right">Devitation</th>
    <th style="text-align: right">Median</th>
    <th style="text-align: right">99th&nbsp;%</th>
  </tr>

  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap; text-align: right">925.19</td>
    <td style="white-space: nowrap; text-align: right">1.08 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;23.66%</td>
    <td style="white-space: nowrap; text-align: right">1.14 ms</td>
    <td style="white-space: nowrap; text-align: right">1.57 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">498.15</td>
    <td style="white-space: nowrap; text-align: right">2.01 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;5.30%</td>
    <td style="white-space: nowrap; text-align: right">1.96 ms</td>
    <td style="white-space: nowrap; text-align: right">2.33 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">494.10</td>
    <td style="white-space: nowrap; text-align: right">2.02 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;5.98%</td>
    <td style="white-space: nowrap; text-align: right">1.97 ms</td>
    <td style="white-space: nowrap; text-align: right">2.29 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">451.81</td>
    <td style="white-space: nowrap; text-align: right">2.21 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;6.85%</td>
    <td style="white-space: nowrap; text-align: right">2.22 ms</td>
    <td style="white-space: nowrap; text-align: right">2.52 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">278.02</td>
    <td style="white-space: nowrap; text-align: right">3.60 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;3.82%</td>
    <td style="white-space: nowrap; text-align: right">3.53 ms</td>
    <td style="white-space: nowrap; text-align: right">3.98 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">162.97</td>
    <td style="white-space: nowrap; text-align: right">6.14 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.66%</td>
    <td style="white-space: nowrap; text-align: right">6.12 ms</td>
    <td style="white-space: nowrap; text-align: right">6.61 ms</td>
  </tr>

</table>


Run Time Comparison

<table style="width: 1%">
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">925.19</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">498.15</td>
    <td style="white-space: nowrap; text-align: right">1.86x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">494.10</td>
    <td style="white-space: nowrap; text-align: right">1.87x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">451.81</td>
    <td style="white-space: nowrap; text-align: right">2.05x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">278.02</td>
    <td style="white-space: nowrap; text-align: right">3.33x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">162.97</td>
    <td style="white-space: nowrap; text-align: right">5.68x</td>
  </tr>

</table>



Memory Usage

<table style="width: 1%">
  <tr>
    <th>Name</th>
    <th style="text-align: right">Average</th>
    <th style="text-align: right">Factor</th>
  </tr>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap">263.03 KB</td>
    <td>&nbsp;</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap">687.22 KB</td>
    <td>2.61x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">691.28 KB</td>
    <td>2.63x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap">687.16 KB</td>
    <td>2.61x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap">2457.91 KB</td>
    <td>9.34x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap">3736.40 KB</td>
    <td>14.21x</td>
  </tr>
</table>



__Input: GitHub__

Run Time

<table style="width: 1%">
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Average</th>
    <th style="text-align: right">Devitation</th>
    <th style="text-align: right">Median</th>
    <th style="text-align: right">99th&nbsp;%</th>
  </tr>

  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap; text-align: right">2.69 K</td>
    <td style="white-space: nowrap; text-align: right">371.31 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;22.78%</td>
    <td style="white-space: nowrap; text-align: right">347.53 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">689.75 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">2.09 K</td>
    <td style="white-space: nowrap; text-align: right">478.20 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;10.91%</td>
    <td style="white-space: nowrap; text-align: right">477.96 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">598.66 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">2.01 K</td>
    <td style="white-space: nowrap; text-align: right">496.78 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;12.80%</td>
    <td style="white-space: nowrap; text-align: right">489.67 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">644.71 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.81 K</td>
    <td style="white-space: nowrap; text-align: right">553.30 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;9.21%</td>
    <td style="white-space: nowrap; text-align: right">560.47 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">688.84 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.42 K</td>
    <td style="white-space: nowrap; text-align: right">705.18 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;8.92%</td>
    <td style="white-space: nowrap; text-align: right">712.22 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">873.78 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.51 K</td>
    <td style="white-space: nowrap; text-align: right">1944.45 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;4.50%</td>
    <td style="white-space: nowrap; text-align: right">1948.18 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">2155.80 &micro;s</td>
  </tr>

</table>


Run Time Comparison

<table style="width: 1%">
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">2.69 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">2.09 K</td>
    <td style="white-space: nowrap; text-align: right">1.29x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">2.01 K</td>
    <td style="white-space: nowrap; text-align: right">1.34x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.81 K</td>
    <td style="white-space: nowrap; text-align: right">1.49x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.42 K</td>
    <td style="white-space: nowrap; text-align: right">1.9x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.51 K</td>
    <td style="white-space: nowrap; text-align: right">5.24x</td>
  </tr>

</table>



Memory Usage

<table style="width: 1%">
  <tr>
    <th>Name</th>
    <th style="text-align: right">Average</th>
    <th style="text-align: right">Factor</th>
  </tr>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap">41.91 KB</td>
    <td>&nbsp;</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">122.89 KB</td>
    <td>2.93x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap">122.80 KB</td>
    <td>2.93x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap">122.73 KB</td>
    <td>2.93x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap">425.48 KB</td>
    <td>10.15x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap">1190.20 KB</td>
    <td>28.4x</td>
  </tr>
</table>



__Input: GovTrack__

Run Time

<table style="width: 1%">
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Average</th>
    <th style="text-align: right">Devitation</th>
    <th style="text-align: right">Median</th>
    <th style="text-align: right">99th&nbsp;%</th>
  </tr>

  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap; text-align: right">27.04</td>
    <td style="white-space: nowrap; text-align: right">36.99 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.40%</td>
    <td style="white-space: nowrap; text-align: right">36.87 ms</td>
    <td style="white-space: nowrap; text-align: right">40.30 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">17.02</td>
    <td style="white-space: nowrap; text-align: right">58.77 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.68%</td>
    <td style="white-space: nowrap; text-align: right">58.88 ms</td>
    <td style="white-space: nowrap; text-align: right">63.23 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">16.79</td>
    <td style="white-space: nowrap; text-align: right">59.56 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.02%</td>
    <td style="white-space: nowrap; text-align: right">59.49 ms</td>
    <td style="white-space: nowrap; text-align: right">62.49 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">15.69</td>
    <td style="white-space: nowrap; text-align: right">63.73 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.14%</td>
    <td style="white-space: nowrap; text-align: right">63.70 ms</td>
    <td style="white-space: nowrap; text-align: right">67.98 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">9.35</td>
    <td style="white-space: nowrap; text-align: right">106.99 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.93%</td>
    <td style="white-space: nowrap; text-align: right">107.42 ms</td>
    <td style="white-space: nowrap; text-align: right">112.36 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">4.40</td>
    <td style="white-space: nowrap; text-align: right">227.07 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.30%</td>
    <td style="white-space: nowrap; text-align: right">227.90 ms</td>
    <td style="white-space: nowrap; text-align: right">232.04 ms</td>
  </tr>

</table>


Run Time Comparison

<table style="width: 1%">
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">27.04</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">17.02</td>
    <td style="white-space: nowrap; text-align: right">1.59x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">16.79</td>
    <td style="white-space: nowrap; text-align: right">1.61x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">15.69</td>
    <td style="white-space: nowrap; text-align: right">1.72x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">9.35</td>
    <td style="white-space: nowrap; text-align: right">2.89x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">4.40</td>
    <td style="white-space: nowrap; text-align: right">6.14x</td>
  </tr>

</table>



Memory Usage

<table style="width: 1%">
  <tr>
    <th>Name</th>
    <th style="text-align: right">Average</th>
    <th style="text-align: right">Factor</th>
  </tr>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap">11.29 MB</td>
    <td>&nbsp;</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap">11.68 MB</td>
    <td>1.03x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">11.78 MB</td>
    <td>1.04x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap">11.68 MB</td>
    <td>1.03x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap">38.05 MB</td>
    <td>3.37x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap">81.29 MB</td>
    <td>7.2x</td>
  </tr>
</table>



__Input: Issue 90__

Run Time

<table style="width: 1%">
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Average</th>
    <th style="text-align: right">Devitation</th>
    <th style="text-align: right">Median</th>
    <th style="text-align: right">99th&nbsp;%</th>
  </tr>

  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap; text-align: right">49.92</td>
    <td style="white-space: nowrap; text-align: right">20.03 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.64%</td>
    <td style="white-space: nowrap; text-align: right">19.95 ms</td>
    <td style="white-space: nowrap; text-align: right">21.76 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">26.62</td>
    <td style="white-space: nowrap; text-align: right">37.57 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;0.98%</td>
    <td style="white-space: nowrap; text-align: right">37.51 ms</td>
    <td style="white-space: nowrap; text-align: right">38.89 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">25.25</td>
    <td style="white-space: nowrap; text-align: right">39.60 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.38%</td>
    <td style="white-space: nowrap; text-align: right">39.45 ms</td>
    <td style="white-space: nowrap; text-align: right">41.49 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">23.25</td>
    <td style="white-space: nowrap; text-align: right">43.01 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;0.76%</td>
    <td style="white-space: nowrap; text-align: right">42.95 ms</td>
    <td style="white-space: nowrap; text-align: right">44.36 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">17.79</td>
    <td style="white-space: nowrap; text-align: right">56.21 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.07%</td>
    <td style="white-space: nowrap; text-align: right">56.06 ms</td>
    <td style="white-space: nowrap; text-align: right">59.39 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">9.92</td>
    <td style="white-space: nowrap; text-align: right">100.77 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.82%</td>
    <td style="white-space: nowrap; text-align: right">100.23 ms</td>
    <td style="white-space: nowrap; text-align: right">122.37 ms</td>
  </tr>

</table>


Run Time Comparison

<table style="width: 1%">
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">49.92</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">26.62</td>
    <td style="white-space: nowrap; text-align: right">1.88x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">25.25</td>
    <td style="white-space: nowrap; text-align: right">1.98x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">23.25</td>
    <td style="white-space: nowrap; text-align: right">2.15x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">17.79</td>
    <td style="white-space: nowrap; text-align: right">2.81x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">9.92</td>
    <td style="white-space: nowrap; text-align: right">5.03x</td>
  </tr>

</table>



Memory Usage

<table style="width: 1%">
  <tr>
    <th>Name</th>
    <th style="text-align: right">Average</th>
    <th style="text-align: right">Factor</th>
  </tr>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap">0.0114 MB</td>
    <td>&nbsp;</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">1.06 MB</td>
    <td>92.92x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap">0.94 MB</td>
    <td>82.47x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap">5.16 MB</td>
    <td>453.09x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap">0.94 MB</td>
    <td>82.46x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap">4.26 MB</td>
    <td>374.05x</td>
  </tr>
</table>



__Input: JSON Generator__

Run Time

<table style="width: 1%">
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Average</th>
    <th style="text-align: right">Devitation</th>
    <th style="text-align: right">Median</th>
    <th style="text-align: right">99th&nbsp;%</th>
  </tr>

  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap; text-align: right">1143.30</td>
    <td style="white-space: nowrap; text-align: right">0.87 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;24.34%</td>
    <td style="white-space: nowrap; text-align: right">0.79 ms</td>
    <td style="white-space: nowrap; text-align: right">1.35 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">707.45</td>
    <td style="white-space: nowrap; text-align: right">1.41 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;6.90%</td>
    <td style="white-space: nowrap; text-align: right">1.44 ms</td>
    <td style="white-space: nowrap; text-align: right">1.67 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">696.88</td>
    <td style="white-space: nowrap; text-align: right">1.43 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;6.90%</td>
    <td style="white-space: nowrap; text-align: right">1.46 ms</td>
    <td style="white-space: nowrap; text-align: right">1.65 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">624.07</td>
    <td style="white-space: nowrap; text-align: right">1.60 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;6.00%</td>
    <td style="white-space: nowrap; text-align: right">1.62 ms</td>
    <td style="white-space: nowrap; text-align: right">1.84 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">435.82</td>
    <td style="white-space: nowrap; text-align: right">2.29 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;6.47%</td>
    <td style="white-space: nowrap; text-align: right">2.28 ms</td>
    <td style="white-space: nowrap; text-align: right">2.58 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">185.81</td>
    <td style="white-space: nowrap; text-align: right">5.38 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.71%</td>
    <td style="white-space: nowrap; text-align: right">5.38 ms</td>
    <td style="white-space: nowrap; text-align: right">5.82 ms</td>
  </tr>

</table>


Run Time Comparison

<table style="width: 1%">
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">1143.30</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">707.45</td>
    <td style="white-space: nowrap; text-align: right">1.62x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">696.88</td>
    <td style="white-space: nowrap; text-align: right">1.64x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">624.07</td>
    <td style="white-space: nowrap; text-align: right">1.83x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">435.82</td>
    <td style="white-space: nowrap; text-align: right">2.62x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">185.81</td>
    <td style="white-space: nowrap; text-align: right">6.15x</td>
  </tr>

</table>



Memory Usage

<table style="width: 1%">
  <tr>
    <th>Name</th>
    <th style="text-align: right">Average</th>
    <th style="text-align: right">Factor</th>
  </tr>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap">207.80 KB</td>
    <td>&nbsp;</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">481.30 KB</td>
    <td>2.32x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap">481.37 KB</td>
    <td>2.32x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap">481.23 KB</td>
    <td>2.32x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap">1475.59 KB</td>
    <td>7.1x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap">3113.34 KB</td>
    <td>14.98x</td>
  </tr>
</table>



__Input: JSON Generator (Pretty)__

Run Time

<table style="width: 1%">
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Average</th>
    <th style="text-align: right">Devitation</th>
    <th style="text-align: right">Median</th>
    <th style="text-align: right">99th&nbsp;%</th>
  </tr>

  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap; text-align: right">698.94</td>
    <td style="white-space: nowrap; text-align: right">1.43 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;10.57%</td>
    <td style="white-space: nowrap; text-align: right">1.43 ms</td>
    <td style="white-space: nowrap; text-align: right">1.77 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">616.18</td>
    <td style="white-space: nowrap; text-align: right">1.62 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;8.20%</td>
    <td style="white-space: nowrap; text-align: right">1.58 ms</td>
    <td style="white-space: nowrap; text-align: right">1.92 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">591.72</td>
    <td style="white-space: nowrap; text-align: right">1.69 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;5.55%</td>
    <td style="white-space: nowrap; text-align: right">1.71 ms</td>
    <td style="white-space: nowrap; text-align: right">1.91 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">539.94</td>
    <td style="white-space: nowrap; text-align: right">1.85 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;4.99%</td>
    <td style="white-space: nowrap; text-align: right">1.85 ms</td>
    <td style="white-space: nowrap; text-align: right">2.07 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">400.04</td>
    <td style="white-space: nowrap; text-align: right">2.50 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;4.56%</td>
    <td style="white-space: nowrap; text-align: right">2.48 ms</td>
    <td style="white-space: nowrap; text-align: right">2.78 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">176.01</td>
    <td style="white-space: nowrap; text-align: right">5.68 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.47%</td>
    <td style="white-space: nowrap; text-align: right">5.67 ms</td>
    <td style="white-space: nowrap; text-align: right">6.13 ms</td>
  </tr>

</table>


Run Time Comparison

<table style="width: 1%">
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">698.94</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">616.18</td>
    <td style="white-space: nowrap; text-align: right">1.13x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">591.72</td>
    <td style="white-space: nowrap; text-align: right">1.18x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">539.94</td>
    <td style="white-space: nowrap; text-align: right">1.29x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">400.04</td>
    <td style="white-space: nowrap; text-align: right">1.75x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">176.01</td>
    <td style="white-space: nowrap; text-align: right">3.97x</td>
  </tr>

</table>



Memory Usage

<table style="width: 1%">
  <tr>
    <th>Name</th>
    <th style="text-align: right">Average</th>
    <th style="text-align: right">Factor</th>
  </tr>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap">257.05 KB</td>
    <td>&nbsp;</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">481.30 KB</td>
    <td>1.87x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap">481.30 KB</td>
    <td>1.87x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap">481.30 KB</td>
    <td>1.87x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap">1475.85 KB</td>
    <td>5.74x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap">3113.45 KB</td>
    <td>12.11x</td>
  </tr>
</table>



__Input: Pokedex__

Run Time

<table style="width: 1%">
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Average</th>
    <th style="text-align: right">Devitation</th>
    <th style="text-align: right">Median</th>
    <th style="text-align: right">99th&nbsp;%</th>
  </tr>

  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap; text-align: right">1.36 K</td>
    <td style="white-space: nowrap; text-align: right">737.51 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;25.25%</td>
    <td style="white-space: nowrap; text-align: right">750.61 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">1133.53 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.17 K</td>
    <td style="white-space: nowrap; text-align: right">853.30 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;9.39%</td>
    <td style="white-space: nowrap; text-align: right">856.90 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">1041.57 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.15 K</td>
    <td style="white-space: nowrap; text-align: right">870.52 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;10.57%</td>
    <td style="white-space: nowrap; text-align: right">814.52 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">1075.63 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.04 K</td>
    <td style="white-space: nowrap; text-align: right">960.73 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;8.98%</td>
    <td style="white-space: nowrap; text-align: right">971.29 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">1218.48 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">0.59 K</td>
    <td style="white-space: nowrap; text-align: right">1699.97 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;7.46%</td>
    <td style="white-space: nowrap; text-align: right">1702.86 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">1982.91 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.26 K</td>
    <td style="white-space: nowrap; text-align: right">3916.08 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.01%</td>
    <td style="white-space: nowrap; text-align: right">3903.43 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">4228.32 &micro;s</td>
  </tr>

</table>


Run Time Comparison

<table style="width: 1%">
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">1.36 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.17 K</td>
    <td style="white-space: nowrap; text-align: right">1.16x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.15 K</td>
    <td style="white-space: nowrap; text-align: right">1.18x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.04 K</td>
    <td style="white-space: nowrap; text-align: right">1.3x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">0.59 K</td>
    <td style="white-space: nowrap; text-align: right">2.31x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.26 K</td>
    <td style="white-space: nowrap; text-align: right">5.31x</td>
  </tr>

</table>



Memory Usage

<table style="width: 1%">
  <tr>
    <th>Name</th>
    <th style="text-align: right">Average</th>
    <th style="text-align: right">Factor</th>
  </tr>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap">101.91 KB</td>
    <td>&nbsp;</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">389.74 KB</td>
    <td>3.82x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap">389.66 KB</td>
    <td>3.82x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap">389.81 KB</td>
    <td>3.83x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap">1260.86 KB</td>
    <td>12.37x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap">2566.13 KB</td>
    <td>25.18x</td>
  </tr>
</table>



__Input: UTF-8 escaped__

Run Time

<table style="width: 1%">
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Average</th>
    <th style="text-align: right">Devitation</th>
    <th style="text-align: right">Median</th>
    <th style="text-align: right">99th&nbsp;%</th>
  </tr>

  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap; text-align: right">10.41 K</td>
    <td style="white-space: nowrap; text-align: right">96.02 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;9.31%</td>
    <td style="white-space: nowrap; text-align: right">94.70 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">127.21 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.70 K</td>
    <td style="white-space: nowrap; text-align: right">589.40 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;22.09%</td>
    <td style="white-space: nowrap; text-align: right">581.53 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">871.23 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.69 K</td>
    <td style="white-space: nowrap; text-align: right">593.33 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;23.62%</td>
    <td style="white-space: nowrap; text-align: right">558.74 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">899.45 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.47 K</td>
    <td style="white-space: nowrap; text-align: right">680.73 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;21.03%</td>
    <td style="white-space: nowrap; text-align: right">670.70 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">976.69 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.29 K</td>
    <td style="white-space: nowrap; text-align: right">776.43 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;11.53%</td>
    <td style="white-space: nowrap; text-align: right">774.96 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">980.28 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.16 K</td>
    <td style="white-space: nowrap; text-align: right">861.73 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;15.89%</td>
    <td style="white-space: nowrap; text-align: right">912.12 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">1130.08 &micro;s</td>
  </tr>

</table>


Run Time Comparison

<table style="width: 1%">
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">10.41 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.70 K</td>
    <td style="white-space: nowrap; text-align: right">6.14x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.69 K</td>
    <td style="white-space: nowrap; text-align: right">6.18x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.47 K</td>
    <td style="white-space: nowrap; text-align: right">7.09x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.29 K</td>
    <td style="white-space: nowrap; text-align: right">8.09x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.16 K</td>
    <td style="white-space: nowrap; text-align: right">8.97x</td>
  </tr>

</table>



Memory Usage

<table style="width: 1%">
  <tr>
    <th>Name</th>
    <th style="text-align: right">Average</th>
    <th style="text-align: right">Factor</th>
  </tr>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap">0.0703 KB</td>
    <td>&nbsp;</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap">283.73 KB</td>
    <td>4035.22x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap">283.80 KB</td>
    <td>4036.22x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">430.70 KB</td>
    <td>6125.44x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap">741.67 KB</td>
    <td>10548.22x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap">824.53 KB</td>
    <td>11726.67x</td>
  </tr>
</table>



__Input: UTF-8 unescaped__

Run Time

<table style="width: 1%">
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Average</th>
    <th style="text-align: right">Devitation</th>
    <th style="text-align: right">Median</th>
    <th style="text-align: right">99th&nbsp;%</th>
  </tr>

  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap; text-align: right">18.12 K</td>
    <td style="white-space: nowrap; text-align: right">55.19 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;19.19%</td>
    <td style="white-space: nowrap; text-align: right">54.30 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">76.64 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">10.44 K</td>
    <td style="white-space: nowrap; text-align: right">95.79 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;55.38%</td>
    <td style="white-space: nowrap; text-align: right">86.59 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">395.17 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">10.19 K</td>
    <td style="white-space: nowrap; text-align: right">98.14 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;53.18%</td>
    <td style="white-space: nowrap; text-align: right">89.17 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">406.31 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">9.60 K</td>
    <td style="white-space: nowrap; text-align: right">104.21 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;47.86%</td>
    <td style="white-space: nowrap; text-align: right">94.79 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">396.40 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">9.44 K</td>
    <td style="white-space: nowrap; text-align: right">105.95 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;71.07%</td>
    <td style="white-space: nowrap; text-align: right">90.71 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">545.59 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">6.57 K</td>
    <td style="white-space: nowrap; text-align: right">152.18 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;46.54%</td>
    <td style="white-space: nowrap; text-align: right">127.85 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">498.10 &micro;s</td>
  </tr>

</table>


Run Time Comparison

<table style="width: 1%">
  <tr>
    <th>Name</th>
    <th style="text-align: right">IPS</th>
    <th style="text-align: right">Slower</th>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap;text-align: right">18.12 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">10.44 K</td>
    <td style="white-space: nowrap; text-align: right">1.74x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">10.19 K</td>
    <td style="white-space: nowrap; text-align: right">1.78x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">9.60 K</td>
    <td style="white-space: nowrap; text-align: right">1.89x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">9.44 K</td>
    <td style="white-space: nowrap; text-align: right">1.92x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">6.57 K</td>
    <td style="white-space: nowrap; text-align: right">2.76x</td>
  </tr>

</table>



Memory Usage

<table style="width: 1%">
  <tr>
    <th>Name</th>
    <th style="text-align: right">Average</th>
    <th style="text-align: right">Factor</th>
  </tr>
  <tr>
    <td style="white-space: nowrap">jiffy</td>
    <td style="white-space: nowrap">0.0703 KB</td>
    <td>&nbsp;</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">6.85 KB</td>
    <td>97.44x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap">6.87 KB</td>
    <td>97.67x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap">6.80 KB</td>
    <td>96.67x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap">33.67 KB</td>
    <td>478.89x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap">42.22 KB</td>
    <td>600.44x</td>
  </tr>
</table>