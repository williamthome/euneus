Benchmark

Benchmark run from 2023-11-17 21:17:25.443284Z UTC

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
    <td style="white-space: nowrap; text-align: right">5.98 K</td>
    <td style="white-space: nowrap; text-align: right">167.24 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;44.49%</td>
    <td style="white-space: nowrap; text-align: right">148.73 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">525.20 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">5.59 K</td>
    <td style="white-space: nowrap; text-align: right">179.03 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;45.33%</td>
    <td style="white-space: nowrap; text-align: right">154.38 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">512.95 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">4.86 K</td>
    <td style="white-space: nowrap; text-align: right">205.79 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;29.84%</td>
    <td style="white-space: nowrap; text-align: right">186.79 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">479.75 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">4.25 K</td>
    <td style="white-space: nowrap; text-align: right">235.14 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;32.07%</td>
    <td style="white-space: nowrap; text-align: right">222.02 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">485.72 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.83 K</td>
    <td style="white-space: nowrap; text-align: right">546.65 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;11.94%</td>
    <td style="white-space: nowrap; text-align: right">551.52 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">709.47 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.64 K</td>
    <td style="white-space: nowrap; text-align: right">610.01 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;11.08%</td>
    <td style="white-space: nowrap; text-align: right">596.30 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">766.52 &micro;s</td>
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
    <td style="white-space: nowrap;text-align: right">5.98 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">5.59 K</td>
    <td style="white-space: nowrap; text-align: right">1.07x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">4.86 K</td>
    <td style="white-space: nowrap; text-align: right">1.23x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">4.25 K</td>
    <td style="white-space: nowrap; text-align: right">1.41x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.83 K</td>
    <td style="white-space: nowrap; text-align: right">3.27x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.64 K</td>
    <td style="white-space: nowrap; text-align: right">3.65x</td>
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
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap">151.39 KB</td>
    <td>97.87x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap">311.43 KB</td>
    <td>201.33x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">558.15 KB</td>
    <td>360.82x</td>
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
    <td style="white-space: nowrap; text-align: right">975.66</td>
    <td style="white-space: nowrap; text-align: right">1.02 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;24.71%</td>
    <td style="white-space: nowrap; text-align: right">1.02 ms</td>
    <td style="white-space: nowrap; text-align: right">1.61 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">492.76</td>
    <td style="white-space: nowrap; text-align: right">2.03 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;5.00%</td>
    <td style="white-space: nowrap; text-align: right">1.98 ms</td>
    <td style="white-space: nowrap; text-align: right">2.34 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">467.73</td>
    <td style="white-space: nowrap; text-align: right">2.14 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;7.07%</td>
    <td style="white-space: nowrap; text-align: right">2.15 ms</td>
    <td style="white-space: nowrap; text-align: right">2.45 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">278.18</td>
    <td style="white-space: nowrap; text-align: right">3.59 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;3.91%</td>
    <td style="white-space: nowrap; text-align: right">3.52 ms</td>
    <td style="white-space: nowrap; text-align: right">3.99 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">165.33</td>
    <td style="white-space: nowrap; text-align: right">6.05 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.66%</td>
    <td style="white-space: nowrap; text-align: right">6.02 ms</td>
    <td style="white-space: nowrap; text-align: right">6.50 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">148.98</td>
    <td style="white-space: nowrap; text-align: right">6.71 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.11%</td>
    <td style="white-space: nowrap; text-align: right">6.72 ms</td>
    <td style="white-space: nowrap; text-align: right">7.32 ms</td>
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
    <td style="white-space: nowrap;text-align: right">975.66</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">492.76</td>
    <td style="white-space: nowrap; text-align: right">1.98x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">467.73</td>
    <td style="white-space: nowrap; text-align: right">2.09x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">278.18</td>
    <td style="white-space: nowrap; text-align: right">3.51x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">165.33</td>
    <td style="white-space: nowrap; text-align: right">5.9x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">148.98</td>
    <td style="white-space: nowrap; text-align: right">6.55x</td>
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
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap">3.65 MB</td>
    <td>14.21x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">5.03 MB</td>
    <td>19.57x</td>
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
    <td style="white-space: nowrap; text-align: right">2.70 K</td>
    <td style="white-space: nowrap; text-align: right">371.03 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;22.76%</td>
    <td style="white-space: nowrap; text-align: right">348.45 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">655.11 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">2.02 K</td>
    <td style="white-space: nowrap; text-align: right">494.61 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;12.81%</td>
    <td style="white-space: nowrap; text-align: right">487.71 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">643.43 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.81 K</td>
    <td style="white-space: nowrap; text-align: right">552.82 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;8.91%</td>
    <td style="white-space: nowrap; text-align: right">560.86 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">685.18 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.42 K</td>
    <td style="white-space: nowrap; text-align: right">701.91 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;7.50%</td>
    <td style="white-space: nowrap; text-align: right">709.14 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">845.09 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">0.52 K</td>
    <td style="white-space: nowrap; text-align: right">1921.26 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;3.72%</td>
    <td style="white-space: nowrap; text-align: right">1927.53 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">2112.83 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.52 K</td>
    <td style="white-space: nowrap; text-align: right">1936.19 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;4.29%</td>
    <td style="white-space: nowrap; text-align: right">1947.14 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">2131.16 &micro;s</td>
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
    <td style="white-space: nowrap;text-align: right">2.70 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">2.02 K</td>
    <td style="white-space: nowrap; text-align: right">1.33x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.81 K</td>
    <td style="white-space: nowrap; text-align: right">1.49x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.42 K</td>
    <td style="white-space: nowrap; text-align: right">1.89x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">0.52 K</td>
    <td style="white-space: nowrap; text-align: right">5.18x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.52 K</td>
    <td style="white-space: nowrap; text-align: right">5.22x</td>
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
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap">425.48 KB</td>
    <td>10.15x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">1896.52 KB</td>
    <td>45.26x</td>
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
    <td style="white-space: nowrap; text-align: right">26.88</td>
    <td style="white-space: nowrap; text-align: right">37.20 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.09%</td>
    <td style="white-space: nowrap; text-align: right">37.13 ms</td>
    <td style="white-space: nowrap; text-align: right">39.52 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">17.15</td>
    <td style="white-space: nowrap; text-align: right">58.29 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.93%</td>
    <td style="white-space: nowrap; text-align: right">58.33 ms</td>
    <td style="white-space: nowrap; text-align: right">64.32 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">15.79</td>
    <td style="white-space: nowrap; text-align: right">63.32 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.20%</td>
    <td style="white-space: nowrap; text-align: right">63.06 ms</td>
    <td style="white-space: nowrap; text-align: right">67.23 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">9.36</td>
    <td style="white-space: nowrap; text-align: right">106.80 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.71%</td>
    <td style="white-space: nowrap; text-align: right">106.73 ms</td>
    <td style="white-space: nowrap; text-align: right">113.62 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">4.42</td>
    <td style="white-space: nowrap; text-align: right">226.16 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.32%</td>
    <td style="white-space: nowrap; text-align: right">226.39 ms</td>
    <td style="white-space: nowrap; text-align: right">232.20 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">3.65</td>
    <td style="white-space: nowrap; text-align: right">273.90 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.27%</td>
    <td style="white-space: nowrap; text-align: right">272.56 ms</td>
    <td style="white-space: nowrap; text-align: right">281.44 ms</td>
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
    <td style="white-space: nowrap;text-align: right">26.88</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">17.15</td>
    <td style="white-space: nowrap; text-align: right">1.57x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">15.79</td>
    <td style="white-space: nowrap; text-align: right">1.7x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">9.36</td>
    <td style="white-space: nowrap; text-align: right">2.87x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">4.42</td>
    <td style="white-space: nowrap; text-align: right">6.08x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">3.65</td>
    <td style="white-space: nowrap; text-align: right">7.36x</td>
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
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap">81.29 MB</td>
    <td>7.2x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">147.57 MB</td>
    <td>13.07x</td>
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
    <td style="white-space: nowrap; text-align: right">49.80</td>
    <td style="white-space: nowrap; text-align: right">20.08 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.11%</td>
    <td style="white-space: nowrap; text-align: right">19.95 ms</td>
    <td style="white-space: nowrap; text-align: right">22.42 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">25.88</td>
    <td style="white-space: nowrap; text-align: right">38.64 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.90%</td>
    <td style="white-space: nowrap; text-align: right">38.41 ms</td>
    <td style="white-space: nowrap; text-align: right">40.50 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">25.26</td>
    <td style="white-space: nowrap; text-align: right">39.59 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.32%</td>
    <td style="white-space: nowrap; text-align: right">39.45 ms</td>
    <td style="white-space: nowrap; text-align: right">41.36 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">23.27</td>
    <td style="white-space: nowrap; text-align: right">42.97 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;0.89%</td>
    <td style="white-space: nowrap; text-align: right">42.88 ms</td>
    <td style="white-space: nowrap; text-align: right">44.49 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">17.99</td>
    <td style="white-space: nowrap; text-align: right">55.60 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;0.72%</td>
    <td style="white-space: nowrap; text-align: right">55.48 ms</td>
    <td style="white-space: nowrap; text-align: right">57.25 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">9.87</td>
    <td style="white-space: nowrap; text-align: right">101.36 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;0.55%</td>
    <td style="white-space: nowrap; text-align: right">101.26 ms</td>
    <td style="white-space: nowrap; text-align: right">102.66 ms</td>
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
    <td style="white-space: nowrap;text-align: right">49.80</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">25.88</td>
    <td style="white-space: nowrap; text-align: right">1.92x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">25.26</td>
    <td style="white-space: nowrap; text-align: right">1.97x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">23.27</td>
    <td style="white-space: nowrap; text-align: right">2.14x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">17.99</td>
    <td style="white-space: nowrap; text-align: right">2.77x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">9.87</td>
    <td style="white-space: nowrap; text-align: right">5.05x</td>
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
    <td style="white-space: nowrap">1.23 MB</td>
    <td>107.87x</td>
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
    <td style="white-space: nowrap; text-align: right">1156.93</td>
    <td style="white-space: nowrap; text-align: right">0.86 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;24.00%</td>
    <td style="white-space: nowrap; text-align: right">0.78 ms</td>
    <td style="white-space: nowrap; text-align: right">1.39 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">691.15</td>
    <td style="white-space: nowrap; text-align: right">1.45 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;11.05%</td>
    <td style="white-space: nowrap; text-align: right">1.46 ms</td>
    <td style="white-space: nowrap; text-align: right">1.83 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">622.00</td>
    <td style="white-space: nowrap; text-align: right">1.61 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;8.10%</td>
    <td style="white-space: nowrap; text-align: right">1.62 ms</td>
    <td style="white-space: nowrap; text-align: right">1.99 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">437.37</td>
    <td style="white-space: nowrap; text-align: right">2.29 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;5.06%</td>
    <td style="white-space: nowrap; text-align: right">2.29 ms</td>
    <td style="white-space: nowrap; text-align: right">2.57 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">188.78</td>
    <td style="white-space: nowrap; text-align: right">5.30 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.86%</td>
    <td style="white-space: nowrap; text-align: right">5.29 ms</td>
    <td style="white-space: nowrap; text-align: right">5.87 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">150.94</td>
    <td style="white-space: nowrap; text-align: right">6.62 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.00%</td>
    <td style="white-space: nowrap; text-align: right">6.61 ms</td>
    <td style="white-space: nowrap; text-align: right">7.11 ms</td>
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
    <td style="white-space: nowrap;text-align: right">1156.93</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">691.15</td>
    <td style="white-space: nowrap; text-align: right">1.67x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">622.00</td>
    <td style="white-space: nowrap; text-align: right">1.86x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">437.37</td>
    <td style="white-space: nowrap; text-align: right">2.65x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">188.78</td>
    <td style="white-space: nowrap; text-align: right">6.13x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">150.94</td>
    <td style="white-space: nowrap; text-align: right">7.66x</td>
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
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap">3.04 MB</td>
    <td>14.98x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">5.68 MB</td>
    <td>28.0x</td>
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
    <td style="white-space: nowrap; text-align: right">692.32</td>
    <td style="white-space: nowrap; text-align: right">1.44 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;11.56%</td>
    <td style="white-space: nowrap; text-align: right">1.44 ms</td>
    <td style="white-space: nowrap; text-align: right">1.81 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">591.86</td>
    <td style="white-space: nowrap; text-align: right">1.69 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;5.56%</td>
    <td style="white-space: nowrap; text-align: right">1.71 ms</td>
    <td style="white-space: nowrap; text-align: right">1.91 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">541.94</td>
    <td style="white-space: nowrap; text-align: right">1.85 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;4.97%</td>
    <td style="white-space: nowrap; text-align: right">1.84 ms</td>
    <td style="white-space: nowrap; text-align: right">2.06 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">402.62</td>
    <td style="white-space: nowrap; text-align: right">2.48 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;4.59%</td>
    <td style="white-space: nowrap; text-align: right">2.48 ms</td>
    <td style="white-space: nowrap; text-align: right">2.75 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">179.11</td>
    <td style="white-space: nowrap; text-align: right">5.58 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.77%</td>
    <td style="white-space: nowrap; text-align: right">5.57 ms</td>
    <td style="white-space: nowrap; text-align: right">6.03 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">144.80</td>
    <td style="white-space: nowrap; text-align: right">6.91 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.03%</td>
    <td style="white-space: nowrap; text-align: right">6.89 ms</td>
    <td style="white-space: nowrap; text-align: right">7.47 ms</td>
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
    <td style="white-space: nowrap;text-align: right">692.32</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">591.86</td>
    <td style="white-space: nowrap; text-align: right">1.17x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">541.94</td>
    <td style="white-space: nowrap; text-align: right">1.28x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">402.62</td>
    <td style="white-space: nowrap; text-align: right">1.72x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">179.11</td>
    <td style="white-space: nowrap; text-align: right">3.87x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">144.80</td>
    <td style="white-space: nowrap; text-align: right">4.78x</td>
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
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap">3.04 MB</td>
    <td>12.11x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">5.65 MB</td>
    <td>22.53x</td>
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
    <td style="white-space: nowrap; text-align: right">1.35 K</td>
    <td style="white-space: nowrap; text-align: right">0.74 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;24.09%</td>
    <td style="white-space: nowrap; text-align: right">0.76 ms</td>
    <td style="white-space: nowrap; text-align: right">1.06 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.16 K</td>
    <td style="white-space: nowrap; text-align: right">0.86 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;10.29%</td>
    <td style="white-space: nowrap; text-align: right">0.81 ms</td>
    <td style="white-space: nowrap; text-align: right">1.07 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.06 K</td>
    <td style="white-space: nowrap; text-align: right">0.95 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;8.14%</td>
    <td style="white-space: nowrap; text-align: right">0.96 ms</td>
    <td style="white-space: nowrap; text-align: right">1.17 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">0.59 K</td>
    <td style="white-space: nowrap; text-align: right">1.71 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;7.53%</td>
    <td style="white-space: nowrap; text-align: right">1.72 ms</td>
    <td style="white-space: nowrap; text-align: right">2.00 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.25 K</td>
    <td style="white-space: nowrap; text-align: right">3.93 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.00%</td>
    <td style="white-space: nowrap; text-align: right">3.92 ms</td>
    <td style="white-space: nowrap; text-align: right">4.27 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">0.181 K</td>
    <td style="white-space: nowrap; text-align: right">5.51 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.75%</td>
    <td style="white-space: nowrap; text-align: right">5.50 ms</td>
    <td style="white-space: nowrap; text-align: right">5.86 ms</td>
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
    <td style="white-space: nowrap;text-align: right">1.35 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.16 K</td>
    <td style="white-space: nowrap; text-align: right">1.16x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.06 K</td>
    <td style="white-space: nowrap; text-align: right">1.27x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">0.59 K</td>
    <td style="white-space: nowrap; text-align: right">2.3x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.25 K</td>
    <td style="white-space: nowrap; text-align: right">5.29x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">0.181 K</td>
    <td style="white-space: nowrap; text-align: right">7.42x</td>
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
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap">2.51 MB</td>
    <td>25.18x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">4.46 MB</td>
    <td>44.79x</td>
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
    <td style="white-space: nowrap; text-align: right">96.25 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;12.22%</td>
    <td style="white-space: nowrap; text-align: right">94.84 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">129.35 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.71 K</td>
    <td style="white-space: nowrap; text-align: right">586.17 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;21.29%</td>
    <td style="white-space: nowrap; text-align: right">575.46 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">869.53 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.70 K</td>
    <td style="white-space: nowrap; text-align: right">586.84 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;22.21%</td>
    <td style="white-space: nowrap; text-align: right">561.49 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">869.64 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.46 K</td>
    <td style="white-space: nowrap; text-align: right">686.73 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;20.17%</td>
    <td style="white-space: nowrap; text-align: right">677.30 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">970.88 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.31 K</td>
    <td style="white-space: nowrap; text-align: right">765.71 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;12.43%</td>
    <td style="white-space: nowrap; text-align: right">764.70 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">981.33 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.15 K</td>
    <td style="white-space: nowrap; text-align: right">867.47 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;15.64%</td>
    <td style="white-space: nowrap; text-align: right">930.06 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">1134.91 &micro;s</td>
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
    <td style="white-space: nowrap; text-align: right">1.71 K</td>
    <td style="white-space: nowrap; text-align: right">6.09x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.70 K</td>
    <td style="white-space: nowrap; text-align: right">6.1x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.46 K</td>
    <td style="white-space: nowrap; text-align: right">7.13x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.31 K</td>
    <td style="white-space: nowrap; text-align: right">7.96x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.15 K</td>
    <td style="white-space: nowrap; text-align: right">9.01x</td>
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
    <td style="white-space: nowrap; text-align: right">18.02 K</td>
    <td style="white-space: nowrap; text-align: right">55.48 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;16.16%</td>
    <td style="white-space: nowrap; text-align: right">54.36 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">80.25 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">10.50 K</td>
    <td style="white-space: nowrap; text-align: right">95.20 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;53.50%</td>
    <td style="white-space: nowrap; text-align: right">86.98 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">383.25 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">10.23 K</td>
    <td style="white-space: nowrap; text-align: right">97.77 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;51.42%</td>
    <td style="white-space: nowrap; text-align: right">89.06 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">392.61 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">9.64 K</td>
    <td style="white-space: nowrap; text-align: right">103.74 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;46.94%</td>
    <td style="white-space: nowrap; text-align: right">95.16 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">391.26 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">9.49 K</td>
    <td style="white-space: nowrap; text-align: right">105.42 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;71.47%</td>
    <td style="white-space: nowrap; text-align: right">89.06 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">550.63 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">6.57 K</td>
    <td style="white-space: nowrap; text-align: right">152.10 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;46.46%</td>
    <td style="white-space: nowrap; text-align: right">126.59 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">499.30 &micro;s</td>
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
    <td style="white-space: nowrap;text-align: right">18.02 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">10.50 K</td>
    <td style="white-space: nowrap; text-align: right">1.72x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">10.23 K</td>
    <td style="white-space: nowrap; text-align: right">1.76x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">9.64 K</td>
    <td style="white-space: nowrap; text-align: right">1.87x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">9.49 K</td>
    <td style="white-space: nowrap; text-align: right">1.9x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">6.57 K</td>
    <td style="white-space: nowrap; text-align: right">2.74x</td>
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