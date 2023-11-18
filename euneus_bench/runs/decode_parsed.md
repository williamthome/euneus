Benchmark

Benchmark run from 2023-11-17 20:22:51.116941Z UTC

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
    <td style="white-space: nowrap; text-align: right">5.95 K</td>
    <td style="white-space: nowrap; text-align: right">167.96 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;43.92%</td>
    <td style="white-space: nowrap; text-align: right">149.35 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">521.70 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">5.58 K</td>
    <td style="white-space: nowrap; text-align: right">179.29 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;45.86%</td>
    <td style="white-space: nowrap; text-align: right">153.80 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">515.69 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">5.44 K</td>
    <td style="white-space: nowrap; text-align: right">183.74 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;37.67%</td>
    <td style="white-space: nowrap; text-align: right">164.39 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">496.41 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">4.89 K</td>
    <td style="white-space: nowrap; text-align: right">204.52 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;30.62%</td>
    <td style="white-space: nowrap; text-align: right">185.62 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">490.44 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">3.95 K</td>
    <td style="white-space: nowrap; text-align: right">253.03 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;27.19%</td>
    <td style="white-space: nowrap; text-align: right">242.52 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">485.55 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.79 K</td>
    <td style="white-space: nowrap; text-align: right">559.05 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;11.82%</td>
    <td style="white-space: nowrap; text-align: right">562.32 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">721.35 &micro;s</td>
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
    <td style="white-space: nowrap;text-align: right">5.95 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">5.58 K</td>
    <td style="white-space: nowrap; text-align: right">1.07x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">5.44 K</td>
    <td style="white-space: nowrap; text-align: right">1.09x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">4.89 K</td>
    <td style="white-space: nowrap; text-align: right">1.22x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">3.95 K</td>
    <td style="white-space: nowrap; text-align: right">1.51x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.79 K</td>
    <td style="white-space: nowrap; text-align: right">3.33x</td>
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
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap">51.63 KB</td>
    <td>33.37x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">51.55 KB</td>
    <td>33.33x</td>
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
    <td style="white-space: nowrap; text-align: right">969.77</td>
    <td style="white-space: nowrap; text-align: right">1.03 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;21.62%</td>
    <td style="white-space: nowrap; text-align: right">1.09 ms</td>
    <td style="white-space: nowrap; text-align: right">1.44 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">498.92</td>
    <td style="white-space: nowrap; text-align: right">2.00 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;4.73%</td>
    <td style="white-space: nowrap; text-align: right">1.99 ms</td>
    <td style="white-space: nowrap; text-align: right">2.29 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">496.53</td>
    <td style="white-space: nowrap; text-align: right">2.01 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;5.15%</td>
    <td style="white-space: nowrap; text-align: right">1.97 ms</td>
    <td style="white-space: nowrap; text-align: right">2.33 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">462.17</td>
    <td style="white-space: nowrap; text-align: right">2.16 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;7.15%</td>
    <td style="white-space: nowrap; text-align: right">2.17 ms</td>
    <td style="white-space: nowrap; text-align: right">2.48 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">255.41</td>
    <td style="white-space: nowrap; text-align: right">3.92 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;4.84%</td>
    <td style="white-space: nowrap; text-align: right">3.83 ms</td>
    <td style="white-space: nowrap; text-align: right">4.37 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">162.97</td>
    <td style="white-space: nowrap; text-align: right">6.14 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;3.35%</td>
    <td style="white-space: nowrap; text-align: right">6.09 ms</td>
    <td style="white-space: nowrap; text-align: right">6.74 ms</td>
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
    <td style="white-space: nowrap;text-align: right">969.77</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">498.92</td>
    <td style="white-space: nowrap; text-align: right">1.94x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">496.53</td>
    <td style="white-space: nowrap; text-align: right">1.95x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">462.17</td>
    <td style="white-space: nowrap; text-align: right">2.1x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">255.41</td>
    <td style="white-space: nowrap; text-align: right">3.8x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">162.97</td>
    <td style="white-space: nowrap; text-align: right">5.95x</td>
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
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">691.34 KB</td>
    <td>2.63x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap">687.22 KB</td>
    <td>2.61x</td>
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
    <td style="white-space: nowrap; text-align: right">2.68 K</td>
    <td style="white-space: nowrap; text-align: right">373.44 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;22.70%</td>
    <td style="white-space: nowrap; text-align: right">348.50 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">677.92 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">2.00 K</td>
    <td style="white-space: nowrap; text-align: right">499.75 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;16.00%</td>
    <td style="white-space: nowrap; text-align: right">489.36 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">675.72 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.94 K</td>
    <td style="white-space: nowrap; text-align: right">516.44 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;10.38%</td>
    <td style="white-space: nowrap; text-align: right">526.45 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">628.93 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.80 K</td>
    <td style="white-space: nowrap; text-align: right">556.12 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;10.59%</td>
    <td style="white-space: nowrap; text-align: right">562.27 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">725.15 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.29 K</td>
    <td style="white-space: nowrap; text-align: right">774.89 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;7.54%</td>
    <td style="white-space: nowrap; text-align: right">778.68 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">947.69 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.51 K</td>
    <td style="white-space: nowrap; text-align: right">1971.38 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;4.81%</td>
    <td style="white-space: nowrap; text-align: right">1973.84 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">2196.83 &micro;s</td>
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
    <td style="white-space: nowrap;text-align: right">2.68 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">2.00 K</td>
    <td style="white-space: nowrap; text-align: right">1.34x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.94 K</td>
    <td style="white-space: nowrap; text-align: right">1.38x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.80 K</td>
    <td style="white-space: nowrap; text-align: right">1.49x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.29 K</td>
    <td style="white-space: nowrap; text-align: right">2.07x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.51 K</td>
    <td style="white-space: nowrap; text-align: right">5.28x</td>
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
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap">122.80 KB</td>
    <td>2.93x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">122.89 KB</td>
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
    <td style="white-space: nowrap; text-align: right">27.26</td>
    <td style="white-space: nowrap; text-align: right">36.68 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.59%</td>
    <td style="white-space: nowrap; text-align: right">36.59 ms</td>
    <td style="white-space: nowrap; text-align: right">40.61 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">16.96</td>
    <td style="white-space: nowrap; text-align: right">58.97 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.65%</td>
    <td style="white-space: nowrap; text-align: right">58.92 ms</td>
    <td style="white-space: nowrap; text-align: right">63.15 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">15.93</td>
    <td style="white-space: nowrap; text-align: right">62.77 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;0.86%</td>
    <td style="white-space: nowrap; text-align: right">62.66 ms</td>
    <td style="white-space: nowrap; text-align: right">65.70 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">15.71</td>
    <td style="white-space: nowrap; text-align: right">63.65 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;0.90%</td>
    <td style="white-space: nowrap; text-align: right">63.54 ms</td>
    <td style="white-space: nowrap; text-align: right">67.02 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">8.91</td>
    <td style="white-space: nowrap; text-align: right">112.26 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.05%</td>
    <td style="white-space: nowrap; text-align: right">112.98 ms</td>
    <td style="white-space: nowrap; text-align: right">116.74 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">4.34</td>
    <td style="white-space: nowrap; text-align: right">230.57 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.96%</td>
    <td style="white-space: nowrap; text-align: right">229.97 ms</td>
    <td style="white-space: nowrap; text-align: right">239.95 ms</td>
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
    <td style="white-space: nowrap;text-align: right">27.26</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">16.96</td>
    <td style="white-space: nowrap; text-align: right">1.61x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">15.93</td>
    <td style="white-space: nowrap; text-align: right">1.71x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">15.71</td>
    <td style="white-space: nowrap; text-align: right">1.74x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">8.91</td>
    <td style="white-space: nowrap; text-align: right">3.06x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">4.34</td>
    <td style="white-space: nowrap; text-align: right">6.29x</td>
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
    <td style="white-space: nowrap">11.68 MB</td>
    <td>1.03x</td>
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
    <td style="white-space: nowrap; text-align: right">49.90</td>
    <td style="white-space: nowrap; text-align: right">20.04 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.81%</td>
    <td style="white-space: nowrap; text-align: right">19.94 ms</td>
    <td style="white-space: nowrap; text-align: right">22.22 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">25.29</td>
    <td style="white-space: nowrap; text-align: right">39.54 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.07%</td>
    <td style="white-space: nowrap; text-align: right">39.46 ms</td>
    <td style="white-space: nowrap; text-align: right">41.41 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">24.88</td>
    <td style="white-space: nowrap; text-align: right">40.20 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.78%</td>
    <td style="white-space: nowrap; text-align: right">40.08 ms</td>
    <td style="white-space: nowrap; text-align: right">43.51 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">23.14</td>
    <td style="white-space: nowrap; text-align: right">43.21 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.00%</td>
    <td style="white-space: nowrap; text-align: right">43.17 ms</td>
    <td style="white-space: nowrap; text-align: right">45.43 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">17.76</td>
    <td style="white-space: nowrap; text-align: right">56.31 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.25%</td>
    <td style="white-space: nowrap; text-align: right">56.09 ms</td>
    <td style="white-space: nowrap; text-align: right">58.50 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">10.07</td>
    <td style="white-space: nowrap; text-align: right">99.27 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;0.81%</td>
    <td style="white-space: nowrap; text-align: right">99.27 ms</td>
    <td style="white-space: nowrap; text-align: right">102.53 ms</td>
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
    <td style="white-space: nowrap;text-align: right">49.90</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">25.29</td>
    <td style="white-space: nowrap; text-align: right">1.97x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">24.88</td>
    <td style="white-space: nowrap; text-align: right">2.01x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">23.14</td>
    <td style="white-space: nowrap; text-align: right">2.16x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">17.76</td>
    <td style="white-space: nowrap; text-align: right">2.81x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">10.07</td>
    <td style="white-space: nowrap; text-align: right">4.95x</td>
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
    <td style="white-space: nowrap">11.66 KB</td>
    <td>&nbsp;</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap">961.94 KB</td>
    <td>82.47x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">962.78 KB</td>
    <td>82.54x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap">5284.81 KB</td>
    <td>453.09x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap">961.87 KB</td>
    <td>82.46x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap">4362.92 KB</td>
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
    <td style="white-space: nowrap; text-align: right">1121.63</td>
    <td style="white-space: nowrap; text-align: right">0.89 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;24.79%</td>
    <td style="white-space: nowrap; text-align: right">0.80 ms</td>
    <td style="white-space: nowrap; text-align: right">1.44 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">693.50</td>
    <td style="white-space: nowrap; text-align: right">1.44 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;6.82%</td>
    <td style="white-space: nowrap; text-align: right">1.44 ms</td>
    <td style="white-space: nowrap; text-align: right">1.64 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">692.92</td>
    <td style="white-space: nowrap; text-align: right">1.44 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;7.15%</td>
    <td style="white-space: nowrap; text-align: right">1.47 ms</td>
    <td style="white-space: nowrap; text-align: right">1.66 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">627.27</td>
    <td style="white-space: nowrap; text-align: right">1.59 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;6.20%</td>
    <td style="white-space: nowrap; text-align: right">1.61 ms</td>
    <td style="white-space: nowrap; text-align: right">1.83 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">401.43</td>
    <td style="white-space: nowrap; text-align: right">2.49 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;4.28%</td>
    <td style="white-space: nowrap; text-align: right">2.49 ms</td>
    <td style="white-space: nowrap; text-align: right">2.74 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">186.60</td>
    <td style="white-space: nowrap; text-align: right">5.36 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.37%</td>
    <td style="white-space: nowrap; text-align: right">5.35 ms</td>
    <td style="white-space: nowrap; text-align: right">5.72 ms</td>
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
    <td style="white-space: nowrap;text-align: right">1121.63</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">693.50</td>
    <td style="white-space: nowrap; text-align: right">1.62x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">692.92</td>
    <td style="white-space: nowrap; text-align: right">1.62x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">627.27</td>
    <td style="white-space: nowrap; text-align: right">1.79x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">401.43</td>
    <td style="white-space: nowrap; text-align: right">2.79x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">186.60</td>
    <td style="white-space: nowrap; text-align: right">6.01x</td>
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
    <td style="white-space: nowrap">481.23 KB</td>
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
    <td style="white-space: nowrap; text-align: right">690.56</td>
    <td style="white-space: nowrap; text-align: right">1.45 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;11.37%</td>
    <td style="white-space: nowrap; text-align: right">1.45 ms</td>
    <td style="white-space: nowrap; text-align: right">1.81 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">589.37</td>
    <td style="white-space: nowrap; text-align: right">1.70 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;5.52%</td>
    <td style="white-space: nowrap; text-align: right">1.72 ms</td>
    <td style="white-space: nowrap; text-align: right">1.91 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">560.86</td>
    <td style="white-space: nowrap; text-align: right">1.78 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;4.36%</td>
    <td style="white-space: nowrap; text-align: right">1.79 ms</td>
    <td style="white-space: nowrap; text-align: right">2.02 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">544.72</td>
    <td style="white-space: nowrap; text-align: right">1.84 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;4.96%</td>
    <td style="white-space: nowrap; text-align: right">1.84 ms</td>
    <td style="white-space: nowrap; text-align: right">2.06 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">370.06</td>
    <td style="white-space: nowrap; text-align: right">2.70 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;4.28%</td>
    <td style="white-space: nowrap; text-align: right">2.69 ms</td>
    <td style="white-space: nowrap; text-align: right">2.98 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">176.95</td>
    <td style="white-space: nowrap; text-align: right">5.65 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.20%</td>
    <td style="white-space: nowrap; text-align: right">5.65 ms</td>
    <td style="white-space: nowrap; text-align: right">6.02 ms</td>
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
    <td style="white-space: nowrap;text-align: right">690.56</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">589.37</td>
    <td style="white-space: nowrap; text-align: right">1.17x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">560.86</td>
    <td style="white-space: nowrap; text-align: right">1.23x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">544.72</td>
    <td style="white-space: nowrap; text-align: right">1.27x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">370.06</td>
    <td style="white-space: nowrap; text-align: right">1.87x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">176.95</td>
    <td style="white-space: nowrap; text-align: right">3.9x</td>
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
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap">481.30 KB</td>
    <td>1.87x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
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
    <td style="white-space: nowrap; text-align: right">1.31 K</td>
    <td style="white-space: nowrap; text-align: right">763.55 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;26.15%</td>
    <td style="white-space: nowrap; text-align: right">780.74 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">1173.25 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.16 K</td>
    <td style="white-space: nowrap; text-align: right">865.55 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;10.58%</td>
    <td style="white-space: nowrap; text-align: right">812.29 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">1069.65 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.08 K</td>
    <td style="white-space: nowrap; text-align: right">922.28 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;10.31%</td>
    <td style="white-space: nowrap; text-align: right">941.16 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">1144.54 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.04 K</td>
    <td style="white-space: nowrap; text-align: right">958.82 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;8.11%</td>
    <td style="white-space: nowrap; text-align: right">971.41 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">1168.07 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">0.53 K</td>
    <td style="white-space: nowrap; text-align: right">1877.27 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;6.83%</td>
    <td style="white-space: nowrap; text-align: right">1848.79 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">2185.21 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.25 K</td>
    <td style="white-space: nowrap; text-align: right">4014.46 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.03%</td>
    <td style="white-space: nowrap; text-align: right">4002.73 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">4326.15 &micro;s</td>
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
    <td style="white-space: nowrap;text-align: right">1.31 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.16 K</td>
    <td style="white-space: nowrap; text-align: right">1.13x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.08 K</td>
    <td style="white-space: nowrap; text-align: right">1.21x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.04 K</td>
    <td style="white-space: nowrap; text-align: right">1.26x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">0.53 K</td>
    <td style="white-space: nowrap; text-align: right">2.46x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.25 K</td>
    <td style="white-space: nowrap; text-align: right">5.26x</td>
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
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap">389.66 KB</td>
    <td>3.82x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">389.74 KB</td>
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
    <td style="white-space: nowrap; text-align: right">96.07 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;10.05%</td>
    <td style="white-space: nowrap; text-align: right">94.66 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">129.10 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.72 K</td>
    <td style="white-space: nowrap; text-align: right">582.63 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;21.25%</td>
    <td style="white-space: nowrap; text-align: right">574.88 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">859.48 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.71 K</td>
    <td style="white-space: nowrap; text-align: right">585.37 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;22.05%</td>
    <td style="white-space: nowrap; text-align: right">561.23 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">866.81 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.44 K</td>
    <td style="white-space: nowrap; text-align: right">696.23 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;21.62%</td>
    <td style="white-space: nowrap; text-align: right">680.65 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">1034.42 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.29 K</td>
    <td style="white-space: nowrap; text-align: right">775.48 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;11.46%</td>
    <td style="white-space: nowrap; text-align: right">774.62 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">966.61 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.18 K</td>
    <td style="white-space: nowrap; text-align: right">850.20 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;15.56%</td>
    <td style="white-space: nowrap; text-align: right">898.51 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">1139.54 &micro;s</td>
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
    <td style="white-space: nowrap; text-align: right">1.72 K</td>
    <td style="white-space: nowrap; text-align: right">6.06x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.71 K</td>
    <td style="white-space: nowrap; text-align: right">6.09x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.44 K</td>
    <td style="white-space: nowrap; text-align: right">7.25x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.29 K</td>
    <td style="white-space: nowrap; text-align: right">8.07x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.18 K</td>
    <td style="white-space: nowrap; text-align: right">8.85x</td>
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
    <td style="white-space: nowrap; text-align: right">18.10 K</td>
    <td style="white-space: nowrap; text-align: right">55.26 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;14.91%</td>
    <td style="white-space: nowrap; text-align: right">54.34 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">76.86 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">10.22 K</td>
    <td style="white-space: nowrap; text-align: right">97.81 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;52.64%</td>
    <td style="white-space: nowrap; text-align: right">89.02 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">389.92 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">10.10 K</td>
    <td style="white-space: nowrap; text-align: right">99.04 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;52.61%</td>
    <td style="white-space: nowrap; text-align: right">89 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">398.60 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">9.62 K</td>
    <td style="white-space: nowrap; text-align: right">103.96 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;48.38%</td>
    <td style="white-space: nowrap; text-align: right">95.11 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">402.37 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">9.42 K</td>
    <td style="white-space: nowrap; text-align: right">106.17 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;71.31%</td>
    <td style="white-space: nowrap; text-align: right">90.57 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">557.20 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">6.57 K</td>
    <td style="white-space: nowrap; text-align: right">152.10 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;45.98%</td>
    <td style="white-space: nowrap; text-align: right">128.51 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">496.80 &micro;s</td>
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
    <td style="white-space: nowrap;text-align: right">18.10 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">10.22 K</td>
    <td style="white-space: nowrap; text-align: right">1.77x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">10.10 K</td>
    <td style="white-space: nowrap; text-align: right">1.79x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">9.62 K</td>
    <td style="white-space: nowrap; text-align: right">1.88x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">9.42 K</td>
    <td style="white-space: nowrap; text-align: right">1.92x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">6.57 K</td>
    <td style="white-space: nowrap; text-align: right">2.75x</td>
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