Benchmark

Benchmark run from 2023-11-24 19:45:59.146760Z UTC

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
    <td style="white-space: nowrap; text-align: right">5.99 K</td>
    <td style="white-space: nowrap; text-align: right">166.81 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;44.21%</td>
    <td style="white-space: nowrap; text-align: right">148.90 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">525.23 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">5.57 K</td>
    <td style="white-space: nowrap; text-align: right">179.44 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;45.19%</td>
    <td style="white-space: nowrap; text-align: right">154.42 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">511.54 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">4.81 K</td>
    <td style="white-space: nowrap; text-align: right">207.94 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;30.35%</td>
    <td style="white-space: nowrap; text-align: right">186.72 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">487.73 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">4.61 K</td>
    <td style="white-space: nowrap; text-align: right">216.84 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;35.01%</td>
    <td style="white-space: nowrap; text-align: right">190.44 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">497.46 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">4.25 K</td>
    <td style="white-space: nowrap; text-align: right">235.27 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;31.93%</td>
    <td style="white-space: nowrap; text-align: right">222.43 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">486.03 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.83 K</td>
    <td style="white-space: nowrap; text-align: right">547.11 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;12.09%</td>
    <td style="white-space: nowrap; text-align: right">550.49 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">709.50 &micro;s</td>
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
    <td style="white-space: nowrap;text-align: right">5.99 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">5.57 K</td>
    <td style="white-space: nowrap; text-align: right">1.08x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">4.81 K</td>
    <td style="white-space: nowrap; text-align: right">1.25x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">4.61 K</td>
    <td style="white-space: nowrap; text-align: right">1.3x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">4.25 K</td>
    <td style="white-space: nowrap; text-align: right">1.41x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.83 K</td>
    <td style="white-space: nowrap; text-align: right">3.28x</td>
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
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap">51.41 KB</td>
    <td>33.24x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">86.13 KB</td>
    <td>55.68x</td>
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
    <td style="white-space: nowrap; text-align: right">962.95</td>
    <td style="white-space: nowrap; text-align: right">1.04 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;22.02%</td>
    <td style="white-space: nowrap; text-align: right">1.09 ms</td>
    <td style="white-space: nowrap; text-align: right">1.44 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">489.11</td>
    <td style="white-space: nowrap; text-align: right">2.04 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;5.34%</td>
    <td style="white-space: nowrap; text-align: right">2.01 ms</td>
    <td style="white-space: nowrap; text-align: right">2.38 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">468.75</td>
    <td style="white-space: nowrap; text-align: right">2.13 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;7.14%</td>
    <td style="white-space: nowrap; text-align: right">2.14 ms</td>
    <td style="white-space: nowrap; text-align: right">2.43 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">279.45</td>
    <td style="white-space: nowrap; text-align: right">3.58 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;4.03%</td>
    <td style="white-space: nowrap; text-align: right">3.51 ms</td>
    <td style="white-space: nowrap; text-align: right">3.98 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">240.77</td>
    <td style="white-space: nowrap; text-align: right">4.15 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;3.62%</td>
    <td style="white-space: nowrap; text-align: right">4.11 ms</td>
    <td style="white-space: nowrap; text-align: right">4.62 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">165.48</td>
    <td style="white-space: nowrap; text-align: right">6.04 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.59%</td>
    <td style="white-space: nowrap; text-align: right">6.02 ms</td>
    <td style="white-space: nowrap; text-align: right">6.46 ms</td>
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
    <td style="white-space: nowrap;text-align: right">962.95</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">489.11</td>
    <td style="white-space: nowrap; text-align: right">1.97x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">468.75</td>
    <td style="white-space: nowrap; text-align: right">2.05x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">279.45</td>
    <td style="white-space: nowrap; text-align: right">3.45x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">240.77</td>
    <td style="white-space: nowrap; text-align: right">4.0x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">165.48</td>
    <td style="white-space: nowrap; text-align: right">5.82x</td>
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
    <td style="white-space: nowrap">0.26 MB</td>
    <td>&nbsp;</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap">0.67 MB</td>
    <td>2.61x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap">0.67 MB</td>
    <td>2.61x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap">2.40 MB</td>
    <td>9.34x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">2.54 MB</td>
    <td>9.88x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap">3.65 MB</td>
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
    <td style="white-space: nowrap; text-align: right">371.39 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;22.74%</td>
    <td style="white-space: nowrap; text-align: right">347.09 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">687.69 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">2.02 K</td>
    <td style="white-space: nowrap; text-align: right">493.87 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;12.70%</td>
    <td style="white-space: nowrap; text-align: right">486.02 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">642.94 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.73 K</td>
    <td style="white-space: nowrap; text-align: right">578.76 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;9.44%</td>
    <td style="white-space: nowrap; text-align: right">570.31 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">729.56 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.46 K</td>
    <td style="white-space: nowrap; text-align: right">682.71 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;9.38%</td>
    <td style="white-space: nowrap; text-align: right">673.19 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">845.65 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.42 K</td>
    <td style="white-space: nowrap; text-align: right">705.51 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;7.59%</td>
    <td style="white-space: nowrap; text-align: right">712.86 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">848.74 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.52 K</td>
    <td style="white-space: nowrap; text-align: right">1931.25 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;4.44%</td>
    <td style="white-space: nowrap; text-align: right">1942.63 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">2126.53 &micro;s</td>
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
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">2.02 K</td>
    <td style="white-space: nowrap; text-align: right">1.33x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.73 K</td>
    <td style="white-space: nowrap; text-align: right">1.56x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.46 K</td>
    <td style="white-space: nowrap; text-align: right">1.84x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.42 K</td>
    <td style="white-space: nowrap; text-align: right">1.9x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.52 K</td>
    <td style="white-space: nowrap; text-align: right">5.2x</td>
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
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap">122.73 KB</td>
    <td>2.93x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">316.23 KB</td>
    <td>7.55x</td>
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
    <td style="white-space: nowrap; text-align: right">27.20</td>
    <td style="white-space: nowrap; text-align: right">36.76 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.51%</td>
    <td style="white-space: nowrap; text-align: right">36.66 ms</td>
    <td style="white-space: nowrap; text-align: right">40.01 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">17.17</td>
    <td style="white-space: nowrap; text-align: right">58.25 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.15%</td>
    <td style="white-space: nowrap; text-align: right">58.35 ms</td>
    <td style="white-space: nowrap; text-align: right">61.13 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">15.87</td>
    <td style="white-space: nowrap; text-align: right">63.02 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;0.90%</td>
    <td style="white-space: nowrap; text-align: right">62.89 ms</td>
    <td style="white-space: nowrap; text-align: right">65.92 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">9.51</td>
    <td style="white-space: nowrap; text-align: right">105.10 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.58%</td>
    <td style="white-space: nowrap; text-align: right">105.99 ms</td>
    <td style="white-space: nowrap; text-align: right">109.25 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">8.73</td>
    <td style="white-space: nowrap; text-align: right">114.49 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.60%</td>
    <td style="white-space: nowrap; text-align: right">114.38 ms</td>
    <td style="white-space: nowrap; text-align: right">122.51 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">4.42</td>
    <td style="white-space: nowrap; text-align: right">226.15 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.33%</td>
    <td style="white-space: nowrap; text-align: right">226.34 ms</td>
    <td style="white-space: nowrap; text-align: right">231.83 ms</td>
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
    <td style="white-space: nowrap;text-align: right">27.20</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">17.17</td>
    <td style="white-space: nowrap; text-align: right">1.58x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">15.87</td>
    <td style="white-space: nowrap; text-align: right">1.71x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">9.51</td>
    <td style="white-space: nowrap; text-align: right">2.86x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">8.73</td>
    <td style="white-space: nowrap; text-align: right">3.11x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">4.42</td>
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
    <td style="white-space: nowrap">11.29 MB</td>
    <td>&nbsp;</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">Jason</td>
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
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">37.10 MB</td>
    <td>3.29x</td>
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
    <td style="white-space: nowrap; text-align: right">49.71</td>
    <td style="white-space: nowrap; text-align: right">20.12 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.50%</td>
    <td style="white-space: nowrap; text-align: right">19.97 ms</td>
    <td style="white-space: nowrap; text-align: right">22.61 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">25.27</td>
    <td style="white-space: nowrap; text-align: right">39.57 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.31%</td>
    <td style="white-space: nowrap; text-align: right">39.45 ms</td>
    <td style="white-space: nowrap; text-align: right">41.49 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">25.24</td>
    <td style="white-space: nowrap; text-align: right">39.62 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.01%</td>
    <td style="white-space: nowrap; text-align: right">39.71 ms</td>
    <td style="white-space: nowrap; text-align: right">41.40 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">23.30</td>
    <td style="white-space: nowrap; text-align: right">42.91 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;0.88%</td>
    <td style="white-space: nowrap; text-align: right">42.83 ms</td>
    <td style="white-space: nowrap; text-align: right">44.29 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">17.95</td>
    <td style="white-space: nowrap; text-align: right">55.72 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;0.71%</td>
    <td style="white-space: nowrap; text-align: right">55.68 ms</td>
    <td style="white-space: nowrap; text-align: right">57.07 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">9.95</td>
    <td style="white-space: nowrap; text-align: right">100.47 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.27%</td>
    <td style="white-space: nowrap; text-align: right">100.19 ms</td>
    <td style="white-space: nowrap; text-align: right">108.62 ms</td>
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
    <td style="white-space: nowrap;text-align: right">49.71</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">25.27</td>
    <td style="white-space: nowrap; text-align: right">1.97x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">25.24</td>
    <td style="white-space: nowrap; text-align: right">1.97x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">23.30</td>
    <td style="white-space: nowrap; text-align: right">2.13x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">17.95</td>
    <td style="white-space: nowrap; text-align: right">2.77x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">9.95</td>
    <td style="white-space: nowrap; text-align: right">4.99x</td>
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
    <td style="white-space: nowrap">987.23 KB</td>
    <td>84.64x</td>
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
    <td style="white-space: nowrap; text-align: right">1158.94</td>
    <td style="white-space: nowrap; text-align: right">0.86 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;23.91%</td>
    <td style="white-space: nowrap; text-align: right">0.78 ms</td>
    <td style="white-space: nowrap; text-align: right">1.41 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">697.71</td>
    <td style="white-space: nowrap; text-align: right">1.43 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;7.08%</td>
    <td style="white-space: nowrap; text-align: right">1.46 ms</td>
    <td style="white-space: nowrap; text-align: right">1.64 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">626.52</td>
    <td style="white-space: nowrap; text-align: right">1.60 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;6.40%</td>
    <td style="white-space: nowrap; text-align: right">1.61 ms</td>
    <td style="white-space: nowrap; text-align: right">1.83 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">437.07</td>
    <td style="white-space: nowrap; text-align: right">2.29 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;4.85%</td>
    <td style="white-space: nowrap; text-align: right">2.29 ms</td>
    <td style="white-space: nowrap; text-align: right">2.56 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">405.09</td>
    <td style="white-space: nowrap; text-align: right">2.47 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;4.47%</td>
    <td style="white-space: nowrap; text-align: right">2.45 ms</td>
    <td style="white-space: nowrap; text-align: right">2.77 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">184.24</td>
    <td style="white-space: nowrap; text-align: right">5.43 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;6.44%</td>
    <td style="white-space: nowrap; text-align: right">5.39 ms</td>
    <td style="white-space: nowrap; text-align: right">5.95 ms</td>
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
    <td style="white-space: nowrap;text-align: right">1158.94</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">697.71</td>
    <td style="white-space: nowrap; text-align: right">1.66x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">626.52</td>
    <td style="white-space: nowrap; text-align: right">1.85x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">437.07</td>
    <td style="white-space: nowrap; text-align: right">2.65x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">405.09</td>
    <td style="white-space: nowrap; text-align: right">2.86x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">184.24</td>
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
    <td style="white-space: nowrap">0.20 MB</td>
    <td>&nbsp;</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap">0.47 MB</td>
    <td>2.32x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap">0.47 MB</td>
    <td>2.32x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap">1.44 MB</td>
    <td>7.1x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">1.28 MB</td>
    <td>6.3x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap">3.04 MB</td>
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
    <td style="white-space: nowrap; text-align: right">692.19</td>
    <td style="white-space: nowrap; text-align: right">1.44 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;10.75%</td>
    <td style="white-space: nowrap; text-align: right">1.43 ms</td>
    <td style="white-space: nowrap; text-align: right">1.80 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">592.50</td>
    <td style="white-space: nowrap; text-align: right">1.69 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;5.50%</td>
    <td style="white-space: nowrap; text-align: right">1.71 ms</td>
    <td style="white-space: nowrap; text-align: right">1.89 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">542.55</td>
    <td style="white-space: nowrap; text-align: right">1.84 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;5.07%</td>
    <td style="white-space: nowrap; text-align: right">1.84 ms</td>
    <td style="white-space: nowrap; text-align: right">2.08 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">401.14</td>
    <td style="white-space: nowrap; text-align: right">2.49 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;4.81%</td>
    <td style="white-space: nowrap; text-align: right">2.48 ms</td>
    <td style="white-space: nowrap; text-align: right">2.78 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">363.17</td>
    <td style="white-space: nowrap; text-align: right">2.75 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;4.44%</td>
    <td style="white-space: nowrap; text-align: right">2.73 ms</td>
    <td style="white-space: nowrap; text-align: right">3.10 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">175.54</td>
    <td style="white-space: nowrap; text-align: right">5.70 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.75%</td>
    <td style="white-space: nowrap; text-align: right">5.69 ms</td>
    <td style="white-space: nowrap; text-align: right">6.10 ms</td>
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
    <td style="white-space: nowrap;text-align: right">692.19</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">592.50</td>
    <td style="white-space: nowrap; text-align: right">1.17x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">542.55</td>
    <td style="white-space: nowrap; text-align: right">1.28x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">401.14</td>
    <td style="white-space: nowrap; text-align: right">1.73x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">363.17</td>
    <td style="white-space: nowrap; text-align: right">1.91x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">175.54</td>
    <td style="white-space: nowrap; text-align: right">3.94x</td>
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
    <td style="white-space: nowrap">0.25 MB</td>
    <td>&nbsp;</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap">0.47 MB</td>
    <td>1.87x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap">0.47 MB</td>
    <td>1.87x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap">1.44 MB</td>
    <td>5.74x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">1.28 MB</td>
    <td>5.1x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap">3.04 MB</td>
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
    <td style="white-space: nowrap; text-align: right">1.33 K</td>
    <td style="white-space: nowrap; text-align: right">0.75 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;24.51%</td>
    <td style="white-space: nowrap; text-align: right">0.79 ms</td>
    <td style="white-space: nowrap; text-align: right">1.06 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.17 K</td>
    <td style="white-space: nowrap; text-align: right">0.85 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;10.76%</td>
    <td style="white-space: nowrap; text-align: right">0.80 ms</td>
    <td style="white-space: nowrap; text-align: right">1.06 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.05 K</td>
    <td style="white-space: nowrap; text-align: right">0.95 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;8.51%</td>
    <td style="white-space: nowrap; text-align: right">0.96 ms</td>
    <td style="white-space: nowrap; text-align: right">1.18 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">0.59 K</td>
    <td style="white-space: nowrap; text-align: right">1.71 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;7.59%</td>
    <td style="white-space: nowrap; text-align: right">1.70 ms</td>
    <td style="white-space: nowrap; text-align: right">2.00 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">0.48 K</td>
    <td style="white-space: nowrap; text-align: right">2.10 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;5.43%</td>
    <td style="white-space: nowrap; text-align: right">2.09 ms</td>
    <td style="white-space: nowrap; text-align: right">2.43 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.25 K</td>
    <td style="white-space: nowrap; text-align: right">3.94 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.16%</td>
    <td style="white-space: nowrap; text-align: right">3.92 ms</td>
    <td style="white-space: nowrap; text-align: right">4.34 ms</td>
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
    <td style="white-space: nowrap;text-align: right">1.33 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.17 K</td>
    <td style="white-space: nowrap; text-align: right">1.13x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.05 K</td>
    <td style="white-space: nowrap; text-align: right">1.26x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">0.59 K</td>
    <td style="white-space: nowrap; text-align: right">2.27x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">0.48 K</td>
    <td style="white-space: nowrap; text-align: right">2.78x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.25 K</td>
    <td style="white-space: nowrap; text-align: right">5.23x</td>
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
    <td style="white-space: nowrap">0.0995 MB</td>
    <td>&nbsp;</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap">0.38 MB</td>
    <td>3.82x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap">0.38 MB</td>
    <td>3.83x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap">1.23 MB</td>
    <td>12.37x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">1.29 MB</td>
    <td>12.96x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap">2.51 MB</td>
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
    <td style="white-space: nowrap; text-align: right">10.39 K</td>
    <td style="white-space: nowrap; text-align: right">96.28 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;9.92%</td>
    <td style="white-space: nowrap; text-align: right">94.89 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">129.03 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.70 K</td>
    <td style="white-space: nowrap; text-align: right">587.76 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;21.71%</td>
    <td style="white-space: nowrap; text-align: right">584.84 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">869.23 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.69 K</td>
    <td style="white-space: nowrap; text-align: right">591.31 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;21.81%</td>
    <td style="white-space: nowrap; text-align: right">609.69 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">883.39 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.44 K</td>
    <td style="white-space: nowrap; text-align: right">694.80 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;20.80%</td>
    <td style="white-space: nowrap; text-align: right">679.79 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">987.82 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.30 K</td>
    <td style="white-space: nowrap; text-align: right">770.74 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;11.92%</td>
    <td style="white-space: nowrap; text-align: right">770.04 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">970.46 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.17 K</td>
    <td style="white-space: nowrap; text-align: right">851.19 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;15.43%</td>
    <td style="white-space: nowrap; text-align: right">902.63 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">1138.80 &micro;s</td>
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
    <td style="white-space: nowrap;text-align: right">10.39 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.70 K</td>
    <td style="white-space: nowrap; text-align: right">6.1x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.69 K</td>
    <td style="white-space: nowrap; text-align: right">6.14x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.44 K</td>
    <td style="white-space: nowrap; text-align: right">7.22x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.30 K</td>
    <td style="white-space: nowrap; text-align: right">8.01x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.17 K</td>
    <td style="white-space: nowrap; text-align: right">8.84x</td>
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
    <td style="white-space: nowrap">430.97 KB</td>
    <td>6129.33x</td>
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
    <td style="white-space: nowrap; text-align: right">18.08 K</td>
    <td style="white-space: nowrap; text-align: right">55.30 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;19.51%</td>
    <td style="white-space: nowrap; text-align: right">54.30 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">77.63 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">10.34 K</td>
    <td style="white-space: nowrap; text-align: right">96.68 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;52.46%</td>
    <td style="white-space: nowrap; text-align: right">88.24 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">390.27 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">10.21 K</td>
    <td style="white-space: nowrap; text-align: right">97.96 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;51.20%</td>
    <td style="white-space: nowrap; text-align: right">89.34 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">388.14 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">9.65 K</td>
    <td style="white-space: nowrap; text-align: right">103.68 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;47.29%</td>
    <td style="white-space: nowrap; text-align: right">95.05 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">392.01 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">9.45 K</td>
    <td style="white-space: nowrap; text-align: right">105.78 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;70.90%</td>
    <td style="white-space: nowrap; text-align: right">90.52 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">547.02 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">6.57 K</td>
    <td style="white-space: nowrap; text-align: right">152.19 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;46.70%</td>
    <td style="white-space: nowrap; text-align: right">126.61 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">496.78 &micro;s</td>
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
    <td style="white-space: nowrap;text-align: right">18.08 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">10.34 K</td>
    <td style="white-space: nowrap; text-align: right">1.75x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">10.21 K</td>
    <td style="white-space: nowrap; text-align: right">1.77x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">9.65 K</td>
    <td style="white-space: nowrap; text-align: right">1.87x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">9.45 K</td>
    <td style="white-space: nowrap; text-align: right">1.91x</td>
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
    <td style="white-space: nowrap">7.13 KB</td>
    <td>101.33x</td>
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