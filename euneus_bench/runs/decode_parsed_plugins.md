Benchmark

Benchmark run from 2023-11-18 00:44:16.059986Z UTC

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
    <td style="white-space: nowrap; text-align: right">167.12 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;46.27%</td>
    <td style="white-space: nowrap; text-align: right">148.49 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">570.43 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">5.55 K</td>
    <td style="white-space: nowrap; text-align: right">180.30 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;44.96%</td>
    <td style="white-space: nowrap; text-align: right">155.04 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">512.89 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">4.87 K</td>
    <td style="white-space: nowrap; text-align: right">205.41 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;30.31%</td>
    <td style="white-space: nowrap; text-align: right">186.89 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">492.10 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">4.28 K</td>
    <td style="white-space: nowrap; text-align: right">233.58 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;32.17%</td>
    <td style="white-space: nowrap; text-align: right">220.80 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">486.92 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">3.59 K</td>
    <td style="white-space: nowrap; text-align: right">278.88 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;23.27%</td>
    <td style="white-space: nowrap; text-align: right">270.75 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">421.87 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.83 K</td>
    <td style="white-space: nowrap; text-align: right">545.09 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;13.34%</td>
    <td style="white-space: nowrap; text-align: right">544.30 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">720.73 &micro;s</td>
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
    <td style="white-space: nowrap; text-align: right">5.55 K</td>
    <td style="white-space: nowrap; text-align: right">1.08x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">4.87 K</td>
    <td style="white-space: nowrap; text-align: right">1.23x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">4.28 K</td>
    <td style="white-space: nowrap; text-align: right">1.4x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">3.59 K</td>
    <td style="white-space: nowrap; text-align: right">1.67x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.83 K</td>
    <td style="white-space: nowrap; text-align: right">3.26x</td>
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
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">177.80 KB</td>
    <td>114.94x</td>
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
    <td style="white-space: nowrap; text-align: right">990.34</td>
    <td style="white-space: nowrap; text-align: right">1.01 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;21.67%</td>
    <td style="white-space: nowrap; text-align: right">1.05 ms</td>
    <td style="white-space: nowrap; text-align: right">1.40 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">497.98</td>
    <td style="white-space: nowrap; text-align: right">2.01 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;5.54%</td>
    <td style="white-space: nowrap; text-align: right">1.96 ms</td>
    <td style="white-space: nowrap; text-align: right">2.38 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">456.78</td>
    <td style="white-space: nowrap; text-align: right">2.19 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;14.00%</td>
    <td style="white-space: nowrap; text-align: right">2.15 ms</td>
    <td style="white-space: nowrap; text-align: right">3.68 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">278.42</td>
    <td style="white-space: nowrap; text-align: right">3.59 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;4.14%</td>
    <td style="white-space: nowrap; text-align: right">3.58 ms</td>
    <td style="white-space: nowrap; text-align: right">4.04 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">277.71</td>
    <td style="white-space: nowrap; text-align: right">3.60 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;3.77%</td>
    <td style="white-space: nowrap; text-align: right">3.53 ms</td>
    <td style="white-space: nowrap; text-align: right">3.97 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">167.26</td>
    <td style="white-space: nowrap; text-align: right">5.98 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.59%</td>
    <td style="white-space: nowrap; text-align: right">5.95 ms</td>
    <td style="white-space: nowrap; text-align: right">6.40 ms</td>
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
    <td style="white-space: nowrap;text-align: right">990.34</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">497.98</td>
    <td style="white-space: nowrap; text-align: right">1.99x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">456.78</td>
    <td style="white-space: nowrap; text-align: right">2.17x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">278.42</td>
    <td style="white-space: nowrap; text-align: right">3.56x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">277.71</td>
    <td style="white-space: nowrap; text-align: right">3.57x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">167.26</td>
    <td style="white-space: nowrap; text-align: right">5.92x</td>
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
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">2.04 MB</td>
    <td>7.93x</td>
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
    <td style="white-space: nowrap; text-align: right">369.98 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;21.85%</td>
    <td style="white-space: nowrap; text-align: right">346.13 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">631.10 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">2.01 K</td>
    <td style="white-space: nowrap; text-align: right">498.01 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;13.08%</td>
    <td style="white-space: nowrap; text-align: right">489.88 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">670.05 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.82 K</td>
    <td style="white-space: nowrap; text-align: right">550.78 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;8.96%</td>
    <td style="white-space: nowrap; text-align: right">558.53 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">684.80 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.42 K</td>
    <td style="white-space: nowrap; text-align: right">705.96 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;8.15%</td>
    <td style="white-space: nowrap; text-align: right">710.58 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">890.03 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.17 K</td>
    <td style="white-space: nowrap; text-align: right">851.56 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;7.43%</td>
    <td style="white-space: nowrap; text-align: right">829.94 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">1000.56 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.52 K</td>
    <td style="white-space: nowrap; text-align: right">1922.78 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;4.38%</td>
    <td style="white-space: nowrap; text-align: right">1933.87 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">2118.79 &micro;s</td>
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
    <td style="white-space: nowrap; text-align: right">2.01 K</td>
    <td style="white-space: nowrap; text-align: right">1.35x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.82 K</td>
    <td style="white-space: nowrap; text-align: right">1.49x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.42 K</td>
    <td style="white-space: nowrap; text-align: right">1.91x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.17 K</td>
    <td style="white-space: nowrap; text-align: right">2.3x</td>
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
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap">425.48 KB</td>
    <td>10.15x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">525.91 KB</td>
    <td>12.55x</td>
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
    <td style="white-space: nowrap; text-align: right">27.25</td>
    <td style="white-space: nowrap; text-align: right">36.70 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.64%</td>
    <td style="white-space: nowrap; text-align: right">36.59 ms</td>
    <td style="white-space: nowrap; text-align: right">39.87 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">17.27</td>
    <td style="white-space: nowrap; text-align: right">57.91 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.63%</td>
    <td style="white-space: nowrap; text-align: right">57.97 ms</td>
    <td style="white-space: nowrap; text-align: right">61.68 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">15.78</td>
    <td style="white-space: nowrap; text-align: right">63.38 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.06%</td>
    <td style="white-space: nowrap; text-align: right">63.41 ms</td>
    <td style="white-space: nowrap; text-align: right">66.46 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">9.55</td>
    <td style="white-space: nowrap; text-align: right">104.73 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.79%</td>
    <td style="white-space: nowrap; text-align: right">105.78 ms</td>
    <td style="white-space: nowrap; text-align: right">110.95 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">7.82</td>
    <td style="white-space: nowrap; text-align: right">127.93 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;0.53%</td>
    <td style="white-space: nowrap; text-align: right">127.74 ms</td>
    <td style="white-space: nowrap; text-align: right">131.20 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">4.45</td>
    <td style="white-space: nowrap; text-align: right">224.83 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.41%</td>
    <td style="white-space: nowrap; text-align: right">225.09 ms</td>
    <td style="white-space: nowrap; text-align: right">231.70 ms</td>
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
    <td style="white-space: nowrap;text-align: right">27.25</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">17.27</td>
    <td style="white-space: nowrap; text-align: right">1.58x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">15.78</td>
    <td style="white-space: nowrap; text-align: right">1.73x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">9.55</td>
    <td style="white-space: nowrap; text-align: right">2.85x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">7.82</td>
    <td style="white-space: nowrap; text-align: right">3.49x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">4.45</td>
    <td style="white-space: nowrap; text-align: right">6.13x</td>
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
    <td style="white-space: nowrap">48.83 MB</td>
    <td>4.32x</td>
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
    <td style="white-space: nowrap; text-align: right">49.74</td>
    <td style="white-space: nowrap; text-align: right">20.10 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.36%</td>
    <td style="white-space: nowrap; text-align: right">19.97 ms</td>
    <td style="white-space: nowrap; text-align: right">22.56 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">25.26</td>
    <td style="white-space: nowrap; text-align: right">39.59 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.49%</td>
    <td style="white-space: nowrap; text-align: right">39.44 ms</td>
    <td style="white-space: nowrap; text-align: right">42.12 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">25.23</td>
    <td style="white-space: nowrap; text-align: right">39.64 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.08%</td>
    <td style="white-space: nowrap; text-align: right">39.52 ms</td>
    <td style="white-space: nowrap; text-align: right">41.62 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">23.29</td>
    <td style="white-space: nowrap; text-align: right">42.93 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.17%</td>
    <td style="white-space: nowrap; text-align: right">42.82 ms</td>
    <td style="white-space: nowrap; text-align: right">44.34 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">17.98</td>
    <td style="white-space: nowrap; text-align: right">55.61 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;0.72%</td>
    <td style="white-space: nowrap; text-align: right">55.49 ms</td>
    <td style="white-space: nowrap; text-align: right">57.11 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">9.97</td>
    <td style="white-space: nowrap; text-align: right">100.30 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;0.57%</td>
    <td style="white-space: nowrap; text-align: right">100.19 ms</td>
    <td style="white-space: nowrap; text-align: right">103.67 ms</td>
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
    <td style="white-space: nowrap;text-align: right">49.74</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">25.26</td>
    <td style="white-space: nowrap; text-align: right">1.97x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">25.23</td>
    <td style="white-space: nowrap; text-align: right">1.97x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">23.29</td>
    <td style="white-space: nowrap; text-align: right">2.14x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">17.98</td>
    <td style="white-space: nowrap; text-align: right">2.77x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">9.97</td>
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
    <td style="white-space: nowrap">0.0114 MB</td>
    <td>&nbsp;</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap">0.94 MB</td>
    <td>82.47x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">1.12 MB</td>
    <td>98.13x</td>
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
    <td style="white-space: nowrap; text-align: right">1162.25</td>
    <td style="white-space: nowrap; text-align: right">0.86 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;23.98%</td>
    <td style="white-space: nowrap; text-align: right">0.77 ms</td>
    <td style="white-space: nowrap; text-align: right">1.41 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">694.53</td>
    <td style="white-space: nowrap; text-align: right">1.44 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;7.07%</td>
    <td style="white-space: nowrap; text-align: right">1.47 ms</td>
    <td style="white-space: nowrap; text-align: right">1.64 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">625.05</td>
    <td style="white-space: nowrap; text-align: right">1.60 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;5.86%</td>
    <td style="white-space: nowrap; text-align: right">1.62 ms</td>
    <td style="white-space: nowrap; text-align: right">1.83 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">436.44</td>
    <td style="white-space: nowrap; text-align: right">2.29 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;4.72%</td>
    <td style="white-space: nowrap; text-align: right">2.29 ms</td>
    <td style="white-space: nowrap; text-align: right">2.54 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">347.69</td>
    <td style="white-space: nowrap; text-align: right">2.88 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;4.01%</td>
    <td style="white-space: nowrap; text-align: right">2.86 ms</td>
    <td style="white-space: nowrap; text-align: right">3.31 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">191.09</td>
    <td style="white-space: nowrap; text-align: right">5.23 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.52%</td>
    <td style="white-space: nowrap; text-align: right">5.22 ms</td>
    <td style="white-space: nowrap; text-align: right">5.63 ms</td>
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
    <td style="white-space: nowrap;text-align: right">1162.25</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">694.53</td>
    <td style="white-space: nowrap; text-align: right">1.67x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">625.05</td>
    <td style="white-space: nowrap; text-align: right">1.86x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">436.44</td>
    <td style="white-space: nowrap; text-align: right">2.66x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">347.69</td>
    <td style="white-space: nowrap; text-align: right">3.34x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">191.09</td>
    <td style="white-space: nowrap; text-align: right">6.08x</td>
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
    <td style="white-space: nowrap">1.87 MB</td>
    <td>9.23x</td>
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
    <td style="white-space: nowrap; text-align: right">696.27</td>
    <td style="white-space: nowrap; text-align: right">1.44 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;11.40%</td>
    <td style="white-space: nowrap; text-align: right">1.43 ms</td>
    <td style="white-space: nowrap; text-align: right">1.80 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">586.85</td>
    <td style="white-space: nowrap; text-align: right">1.70 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;7.45%</td>
    <td style="white-space: nowrap; text-align: right">1.72 ms</td>
    <td style="white-space: nowrap; text-align: right">1.96 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">541.03</td>
    <td style="white-space: nowrap; text-align: right">1.85 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;4.77%</td>
    <td style="white-space: nowrap; text-align: right">1.85 ms</td>
    <td style="white-space: nowrap; text-align: right">2.06 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">400.67</td>
    <td style="white-space: nowrap; text-align: right">2.50 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;4.56%</td>
    <td style="white-space: nowrap; text-align: right">2.49 ms</td>
    <td style="white-space: nowrap; text-align: right">2.76 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">317.16</td>
    <td style="white-space: nowrap; text-align: right">3.15 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.88%</td>
    <td style="white-space: nowrap; text-align: right">3.14 ms</td>
    <td style="white-space: nowrap; text-align: right">3.43 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">181.31</td>
    <td style="white-space: nowrap; text-align: right">5.52 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.26%</td>
    <td style="white-space: nowrap; text-align: right">5.51 ms</td>
    <td style="white-space: nowrap; text-align: right">5.88 ms</td>
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
    <td style="white-space: nowrap;text-align: right">696.27</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">586.85</td>
    <td style="white-space: nowrap; text-align: right">1.19x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">541.03</td>
    <td style="white-space: nowrap; text-align: right">1.29x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">400.67</td>
    <td style="white-space: nowrap; text-align: right">1.74x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">317.16</td>
    <td style="white-space: nowrap; text-align: right">2.2x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">181.31</td>
    <td style="white-space: nowrap; text-align: right">3.84x</td>
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
    <td style="white-space: nowrap">1.87 MB</td>
    <td>7.46x</td>
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
    <td style="white-space: nowrap; text-align: right">1.35 K</td>
    <td style="white-space: nowrap; text-align: right">0.74 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;24.04%</td>
    <td style="white-space: nowrap; text-align: right">0.76 ms</td>
    <td style="white-space: nowrap; text-align: right">1.06 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.15 K</td>
    <td style="white-space: nowrap; text-align: right">0.87 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;10.30%</td>
    <td style="white-space: nowrap; text-align: right">0.82 ms</td>
    <td style="white-space: nowrap; text-align: right">1.08 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.04 K</td>
    <td style="white-space: nowrap; text-align: right">0.96 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;8.86%</td>
    <td style="white-space: nowrap; text-align: right">0.97 ms</td>
    <td style="white-space: nowrap; text-align: right">1.17 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">0.59 K</td>
    <td style="white-space: nowrap; text-align: right">1.69 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;7.91%</td>
    <td style="white-space: nowrap; text-align: right">1.70 ms</td>
    <td style="white-space: nowrap; text-align: right">1.97 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">0.40 K</td>
    <td style="white-space: nowrap; text-align: right">2.47 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;6.68%</td>
    <td style="white-space: nowrap; text-align: right">2.43 ms</td>
    <td style="white-space: nowrap; text-align: right">2.88 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.26 K</td>
    <td style="white-space: nowrap; text-align: right">3.90 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.95%</td>
    <td style="white-space: nowrap; text-align: right">3.88 ms</td>
    <td style="white-space: nowrap; text-align: right">4.21 ms</td>
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
    <td style="white-space: nowrap; text-align: right">2.29x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">0.40 K</td>
    <td style="white-space: nowrap; text-align: right">3.34x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.26 K</td>
    <td style="white-space: nowrap; text-align: right">5.27x</td>
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
    <td style="white-space: nowrap">1.80 MB</td>
    <td>18.11x</td>
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
    <td style="white-space: nowrap; text-align: right">10.38 K</td>
    <td style="white-space: nowrap; text-align: right">96.33 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;10.14%</td>
    <td style="white-space: nowrap; text-align: right">94.92 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">129.31 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.70 K</td>
    <td style="white-space: nowrap; text-align: right">586.72 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;22.24%</td>
    <td style="white-space: nowrap; text-align: right">557.24 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">862.69 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.70 K</td>
    <td style="white-space: nowrap; text-align: right">588.78 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;22.16%</td>
    <td style="white-space: nowrap; text-align: right">577.72 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">872.45 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.45 K</td>
    <td style="white-space: nowrap; text-align: right">691.84 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;21.03%</td>
    <td style="white-space: nowrap; text-align: right">674.09 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">989.02 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.30 K</td>
    <td style="white-space: nowrap; text-align: right">767.58 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;12.19%</td>
    <td style="white-space: nowrap; text-align: right">764.59 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">981.19 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.18 K</td>
    <td style="white-space: nowrap; text-align: right">850.34 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;15.70%</td>
    <td style="white-space: nowrap; text-align: right">894.73 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">1125.82 &micro;s</td>
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
    <td style="white-space: nowrap;text-align: right">10.38 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.70 K</td>
    <td style="white-space: nowrap; text-align: right">6.09x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.70 K</td>
    <td style="white-space: nowrap; text-align: right">6.11x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.45 K</td>
    <td style="white-space: nowrap; text-align: right">7.18x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.30 K</td>
    <td style="white-space: nowrap; text-align: right">7.97x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.18 K</td>
    <td style="white-space: nowrap; text-align: right">8.83x</td>
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
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap">283.80 KB</td>
    <td>4036.22x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap">283.73 KB</td>
    <td>4035.22x</td>
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
    <td style="white-space: nowrap; text-align: right">18.07 K</td>
    <td style="white-space: nowrap; text-align: right">55.33 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;16.14%</td>
    <td style="white-space: nowrap; text-align: right">54.28 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">80.17 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">10.37 K</td>
    <td style="white-space: nowrap; text-align: right">96.39 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;52.38%</td>
    <td style="white-space: nowrap; text-align: right">88.09 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">385.64 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">10.20 K</td>
    <td style="white-space: nowrap; text-align: right">98.08 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;51.12%</td>
    <td style="white-space: nowrap; text-align: right">89.63 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">397.35 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">9.59 K</td>
    <td style="white-space: nowrap; text-align: right">104.31 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;46.47%</td>
    <td style="white-space: nowrap; text-align: right">95.71 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">390.28 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">9.51 K</td>
    <td style="white-space: nowrap; text-align: right">105.14 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;71.07%</td>
    <td style="white-space: nowrap; text-align: right">88.37 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">541.34 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">6.58 K</td>
    <td style="white-space: nowrap; text-align: right">151.93 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;47.25%</td>
    <td style="white-space: nowrap; text-align: right">126.86 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">507.54 &micro;s</td>
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
    <td style="white-space: nowrap;text-align: right">18.07 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">10.37 K</td>
    <td style="white-space: nowrap; text-align: right">1.74x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">10.20 K</td>
    <td style="white-space: nowrap; text-align: right">1.77x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">9.59 K</td>
    <td style="white-space: nowrap; text-align: right">1.89x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">9.51 K</td>
    <td style="white-space: nowrap; text-align: right">1.9x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">6.58 K</td>
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