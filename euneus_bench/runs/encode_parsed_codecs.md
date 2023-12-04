Benchmark

Benchmark run from 2023-11-24 19:10:07.301687Z UTC

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
    <td style="white-space: nowrap; text-align: right">11.16 K</td>
    <td style="white-space: nowrap; text-align: right">89.63 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;352.91%</td>
    <td style="white-space: nowrap; text-align: right">61.97 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">110.94 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">4.57 K</td>
    <td style="white-space: nowrap; text-align: right">218.79 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;287.10%</td>
    <td style="white-space: nowrap; text-align: right">110.75 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3536.71 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">3.94 K</td>
    <td style="white-space: nowrap; text-align: right">254.01 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;266.56%</td>
    <td style="white-space: nowrap; text-align: right">104.44 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3534.92 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">3.76 K</td>
    <td style="white-space: nowrap; text-align: right">265.88 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;242.07%</td>
    <td style="white-space: nowrap; text-align: right">131.39 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3548.34 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">2.50 K</td>
    <td style="white-space: nowrap; text-align: right">399.54 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;207.92%</td>
    <td style="white-space: nowrap; text-align: right">202.62 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3604.89 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.01 K</td>
    <td style="white-space: nowrap; text-align: right">989.92 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;126.22%</td>
    <td style="white-space: nowrap; text-align: right">484.00 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">4152.43 &micro;s</td>
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
    <td style="white-space: nowrap;text-align: right">11.16 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">4.57 K</td>
    <td style="white-space: nowrap; text-align: right">2.44x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">3.94 K</td>
    <td style="white-space: nowrap; text-align: right">2.83x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">3.76 K</td>
    <td style="white-space: nowrap; text-align: right">2.97x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">2.50 K</td>
    <td style="white-space: nowrap; text-align: right">4.46x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.01 K</td>
    <td style="white-space: nowrap; text-align: right">11.04x</td>
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
    <td style="white-space: nowrap">8.03 KB</td>
    <td>&nbsp;</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap">78.91 KB</td>
    <td>9.82x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">99.90 KB</td>
    <td>12.44x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap">89.41 KB</td>
    <td>11.13x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap">179.23 KB</td>
    <td>22.32x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap">405.79 KB</td>
    <td>50.53x</td>
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
    <td style="white-space: nowrap; text-align: right">1159.45</td>
    <td style="white-space: nowrap; text-align: right">0.86 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;103.36%</td>
    <td style="white-space: nowrap; text-align: right">0.66 ms</td>
    <td style="white-space: nowrap; text-align: right">4.48 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">430.25</td>
    <td style="white-space: nowrap; text-align: right">2.32 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;76.80%</td>
    <td style="white-space: nowrap; text-align: right">1.08 ms</td>
    <td style="white-space: nowrap; text-align: right">4.92 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">401.71</td>
    <td style="white-space: nowrap; text-align: right">2.49 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;73.06%</td>
    <td style="white-space: nowrap; text-align: right">1.23 ms</td>
    <td style="white-space: nowrap; text-align: right">5.42 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">347.78</td>
    <td style="white-space: nowrap; text-align: right">2.88 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;63.51%</td>
    <td style="white-space: nowrap; text-align: right">4.19 ms</td>
    <td style="white-space: nowrap; text-align: right">5.20 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">212.69</td>
    <td style="white-space: nowrap; text-align: right">4.70 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;33.15%</td>
    <td style="white-space: nowrap; text-align: right">5.33 ms</td>
    <td style="white-space: nowrap; text-align: right">8.41 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">79.29</td>
    <td style="white-space: nowrap; text-align: right">12.61 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.83%</td>
    <td style="white-space: nowrap; text-align: right">12.56 ms</td>
    <td style="white-space: nowrap; text-align: right">13.47 ms</td>
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
    <td style="white-space: nowrap;text-align: right">1159.45</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">430.25</td>
    <td style="white-space: nowrap; text-align: right">2.69x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">401.71</td>
    <td style="white-space: nowrap; text-align: right">2.89x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">347.78</td>
    <td style="white-space: nowrap; text-align: right">3.33x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">212.69</td>
    <td style="white-space: nowrap; text-align: right">5.45x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">79.29</td>
    <td style="white-space: nowrap; text-align: right">14.62x</td>
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
    <td style="white-space: nowrap">0.114 MB</td>
    <td>&nbsp;</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap">0.84 MB</td>
    <td>7.38x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap">0.84 MB</td>
    <td>7.38x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">1.06 MB</td>
    <td>9.31x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap">2.19 MB</td>
    <td>19.18x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap">4.81 MB</td>
    <td>42.19x</td>
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
    <td style="white-space: nowrap; text-align: right">3.54 K</td>
    <td style="white-space: nowrap; text-align: right">282.09 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;175.54%</td>
    <td style="white-space: nowrap; text-align: right">216.08 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3909.01 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.52 K</td>
    <td style="white-space: nowrap; text-align: right">659.04 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;158.60%</td>
    <td style="white-space: nowrap; text-align: right">299.66 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3818.23 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.29 K</td>
    <td style="white-space: nowrap; text-align: right">777.66 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;138.73%</td>
    <td style="white-space: nowrap; text-align: right">406.75 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3960.35 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.28 K</td>
    <td style="white-space: nowrap; text-align: right">782.76 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;144.00%</td>
    <td style="white-space: nowrap; text-align: right">373.75 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3831.46 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">0.66 K</td>
    <td style="white-space: nowrap; text-align: right">1513.46 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;96.41%</td>
    <td style="white-space: nowrap; text-align: right">841.94 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">4326.62 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.197 K</td>
    <td style="white-space: nowrap; text-align: right">5066.27 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;3.33%</td>
    <td style="white-space: nowrap; text-align: right">5042.97 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">5569.72 &micro;s</td>
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
    <td style="white-space: nowrap;text-align: right">3.54 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.52 K</td>
    <td style="white-space: nowrap; text-align: right">2.34x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.29 K</td>
    <td style="white-space: nowrap; text-align: right">2.76x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.28 K</td>
    <td style="white-space: nowrap; text-align: right">2.77x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">0.66 K</td>
    <td style="white-space: nowrap; text-align: right">5.37x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.197 K</td>
    <td style="white-space: nowrap; text-align: right">17.96x</td>
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
    <td style="white-space: nowrap">42.85 KB</td>
    <td>&nbsp;</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap">221.14 KB</td>
    <td>5.16x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap">228.15 KB</td>
    <td>5.32x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">294.34 KB</td>
    <td>6.87x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap">682.82 KB</td>
    <td>15.93x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap">1422.55 KB</td>
    <td>33.2x</td>
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
    <td style="white-space: nowrap; text-align: right">52.65</td>
    <td style="white-space: nowrap; text-align: right">18.99 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;9.45%</td>
    <td style="white-space: nowrap; text-align: right">18.58 ms</td>
    <td style="white-space: nowrap; text-align: right">27.46 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">20.02</td>
    <td style="white-space: nowrap; text-align: right">49.94 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;6.13%</td>
    <td style="white-space: nowrap; text-align: right">49.86 ms</td>
    <td style="white-space: nowrap; text-align: right">63.82 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">17.97</td>
    <td style="white-space: nowrap; text-align: right">55.66 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;8.34%</td>
    <td style="white-space: nowrap; text-align: right">56.48 ms</td>
    <td style="white-space: nowrap; text-align: right">63.47 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">16.60</td>
    <td style="white-space: nowrap; text-align: right">60.24 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;42.16%</td>
    <td style="white-space: nowrap; text-align: right">54.22 ms</td>
    <td style="white-space: nowrap; text-align: right">125.66 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">8.54</td>
    <td style="white-space: nowrap; text-align: right">117.12 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;9.64%</td>
    <td style="white-space: nowrap; text-align: right">119.14 ms</td>
    <td style="white-space: nowrap; text-align: right">127.29 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">3.06</td>
    <td style="white-space: nowrap; text-align: right">327.00 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.79%</td>
    <td style="white-space: nowrap; text-align: right">326.38 ms</td>
    <td style="white-space: nowrap; text-align: right">374.24 ms</td>
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
    <td style="white-space: nowrap;text-align: right">52.65</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">20.02</td>
    <td style="white-space: nowrap; text-align: right">2.63x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">17.97</td>
    <td style="white-space: nowrap; text-align: right">2.93x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">16.60</td>
    <td style="white-space: nowrap; text-align: right">3.17x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">8.54</td>
    <td style="white-space: nowrap; text-align: right">6.17x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">3.06</td>
    <td style="white-space: nowrap; text-align: right">17.22x</td>
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
    <td style="white-space: nowrap">3.30 MB</td>
    <td>&nbsp;</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">25.36 MB</td>
    <td>7.67x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap">18.90 MB</td>
    <td>5.72x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap">20.67 MB</td>
    <td>6.26x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap">45.34 MB</td>
    <td>13.72x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap">98.13 MB</td>
    <td>29.7x</td>
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
    <td style="white-space: nowrap; text-align: right">36.57</td>
    <td style="white-space: nowrap; text-align: right">27.35 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.40%</td>
    <td style="white-space: nowrap; text-align: right">27.26 ms</td>
    <td style="white-space: nowrap; text-align: right">29.09 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">27.45</td>
    <td style="white-space: nowrap; text-align: right">36.43 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;5.46%</td>
    <td style="white-space: nowrap; text-align: right">37.42 ms</td>
    <td style="white-space: nowrap; text-align: right">40.50 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">26.06</td>
    <td style="white-space: nowrap; text-align: right">38.37 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;5.76%</td>
    <td style="white-space: nowrap; text-align: right">39.22 ms</td>
    <td style="white-space: nowrap; text-align: right">44.04 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">16.79</td>
    <td style="white-space: nowrap; text-align: right">59.55 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;3.29%</td>
    <td style="white-space: nowrap; text-align: right">60.52 ms</td>
    <td style="white-space: nowrap; text-align: right">63.60 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">16.68</td>
    <td style="white-space: nowrap; text-align: right">59.94 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.99%</td>
    <td style="white-space: nowrap; text-align: right">60.81 ms</td>
    <td style="white-space: nowrap; text-align: right">62.63 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">8.76</td>
    <td style="white-space: nowrap; text-align: right">114.19 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.13%</td>
    <td style="white-space: nowrap; text-align: right">114.60 ms</td>
    <td style="white-space: nowrap; text-align: right">119.79 ms</td>
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
    <td style="white-space: nowrap;text-align: right">36.57</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">27.45</td>
    <td style="white-space: nowrap; text-align: right">1.33x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">26.06</td>
    <td style="white-space: nowrap; text-align: right">1.4x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">16.79</td>
    <td style="white-space: nowrap; text-align: right">2.18x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">16.68</td>
    <td style="white-space: nowrap; text-align: right">2.19x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">8.76</td>
    <td style="white-space: nowrap; text-align: right">4.18x</td>
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
    <td style="white-space: nowrap">0.0125 MB</td>
    <td>&nbsp;</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap">0.81 MB</td>
    <td>65.33x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">1.08 MB</td>
    <td>86.73x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap">0.82 MB</td>
    <td>66.05x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap">2.64 MB</td>
    <td>212.2x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap">4.57 MB</td>
    <td>366.69x</td>
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
    <td style="white-space: nowrap; text-align: right">1179.26</td>
    <td style="white-space: nowrap; text-align: right">0.85 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;95.28%</td>
    <td style="white-space: nowrap; text-align: right">0.67 ms</td>
    <td style="white-space: nowrap; text-align: right">4.58 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">419.31</td>
    <td style="white-space: nowrap; text-align: right">2.38 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;74.21%</td>
    <td style="white-space: nowrap; text-align: right">1.18 ms</td>
    <td style="white-space: nowrap; text-align: right">4.89 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">410.98</td>
    <td style="white-space: nowrap; text-align: right">2.43 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;80.35%</td>
    <td style="white-space: nowrap; text-align: right">1.06 ms</td>
    <td style="white-space: nowrap; text-align: right">7.74 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">404.67</td>
    <td style="white-space: nowrap; text-align: right">2.47 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;74.73%</td>
    <td style="white-space: nowrap; text-align: right">1.25 ms</td>
    <td style="white-space: nowrap; text-align: right">5.90 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">271.34</td>
    <td style="white-space: nowrap; text-align: right">3.69 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;49.18%</td>
    <td style="white-space: nowrap; text-align: right">4.81 ms</td>
    <td style="white-space: nowrap; text-align: right">8.08 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">98.15</td>
    <td style="white-space: nowrap; text-align: right">10.19 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;23.08%</td>
    <td style="white-space: nowrap; text-align: right">8.89 ms</td>
    <td style="white-space: nowrap; text-align: right">13.16 ms</td>
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
    <td style="white-space: nowrap;text-align: right">1179.26</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">419.31</td>
    <td style="white-space: nowrap; text-align: right">2.81x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">410.98</td>
    <td style="white-space: nowrap; text-align: right">2.87x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">404.67</td>
    <td style="white-space: nowrap; text-align: right">2.91x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">271.34</td>
    <td style="white-space: nowrap; text-align: right">4.35x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">98.15</td>
    <td style="white-space: nowrap; text-align: right">12.02x</td>
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
    <td style="white-space: nowrap">110.68 KB</td>
    <td>&nbsp;</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">957.37 KB</td>
    <td>8.65x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap">778.17 KB</td>
    <td>7.03x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap">769.38 KB</td>
    <td>6.95x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap">1711.06 KB</td>
    <td>15.46x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap">3601.87 KB</td>
    <td>32.54x</td>
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
    <td style="white-space: nowrap; text-align: right">1682.07</td>
    <td style="white-space: nowrap; text-align: right">0.59 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;117.24%</td>
    <td style="white-space: nowrap; text-align: right">0.46 ms</td>
    <td style="white-space: nowrap; text-align: right">4.05 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">628.70</td>
    <td style="white-space: nowrap; text-align: right">1.59 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;102.13%</td>
    <td style="white-space: nowrap; text-align: right">0.70 ms</td>
    <td style="white-space: nowrap; text-align: right">4.41 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">611.61</td>
    <td style="white-space: nowrap; text-align: right">1.64 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;97.86%</td>
    <td style="white-space: nowrap; text-align: right">0.77 ms</td>
    <td style="white-space: nowrap; text-align: right">4.43 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">489.98</td>
    <td style="white-space: nowrap; text-align: right">2.04 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;88.12%</td>
    <td style="white-space: nowrap; text-align: right">0.87 ms</td>
    <td style="white-space: nowrap; text-align: right">4.97 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">320.34</td>
    <td style="white-space: nowrap; text-align: right">3.12 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;58.61%</td>
    <td style="white-space: nowrap; text-align: right">4.37 ms</td>
    <td style="white-space: nowrap; text-align: right">7.83 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">108.05</td>
    <td style="white-space: nowrap; text-align: right">9.25 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;24.24%</td>
    <td style="white-space: nowrap; text-align: right">7.67 ms</td>
    <td style="white-space: nowrap; text-align: right">13.97 ms</td>
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
    <td style="white-space: nowrap;text-align: right">1682.07</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">628.70</td>
    <td style="white-space: nowrap; text-align: right">2.68x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">611.61</td>
    <td style="white-space: nowrap; text-align: right">2.75x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">489.98</td>
    <td style="white-space: nowrap; text-align: right">3.43x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">320.34</td>
    <td style="white-space: nowrap; text-align: right">5.25x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">108.05</td>
    <td style="white-space: nowrap; text-align: right">15.57x</td>
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
    <td style="white-space: nowrap">51.82 KB</td>
    <td>&nbsp;</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap">658.96 KB</td>
    <td>12.72x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap">670.49 KB</td>
    <td>12.94x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">826.38 KB</td>
    <td>15.95x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap">1490.80 KB</td>
    <td>28.77x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap">3532.86 KB</td>
    <td>68.18x</td>
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
    <td style="white-space: nowrap; text-align: right">14.29 K</td>
    <td style="white-space: nowrap; text-align: right">70.00 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;32.99%</td>
    <td style="white-space: nowrap; text-align: right">68.83 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">92.27 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">10.10 K</td>
    <td style="white-space: nowrap; text-align: right">99.01 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;254.94%</td>
    <td style="white-space: nowrap; text-align: right">79.89 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">120.69 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">9.53 K</td>
    <td style="white-space: nowrap; text-align: right">104.96 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;253.78%</td>
    <td style="white-space: nowrap; text-align: right">82.84 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">130.21 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">9.08 K</td>
    <td style="white-space: nowrap; text-align: right">110.12 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;223.33%</td>
    <td style="white-space: nowrap; text-align: right">91.30 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">133.21 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">4.77 K</td>
    <td style="white-space: nowrap; text-align: right">209.74 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;215.15%</td>
    <td style="white-space: nowrap; text-align: right">153.19 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3606.07 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.84 K</td>
    <td style="white-space: nowrap; text-align: right">543.04 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;172.96%</td>
    <td style="white-space: nowrap; text-align: right">260.68 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3477.35 &micro;s</td>
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
    <td style="white-space: nowrap;text-align: right">14.29 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">10.10 K</td>
    <td style="white-space: nowrap; text-align: right">1.41x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">9.53 K</td>
    <td style="white-space: nowrap; text-align: right">1.5x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">9.08 K</td>
    <td style="white-space: nowrap; text-align: right">1.57x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">4.77 K</td>
    <td style="white-space: nowrap; text-align: right">3.0x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.84 K</td>
    <td style="white-space: nowrap; text-align: right">7.76x</td>
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
    <td style="white-space: nowrap">0.70 KB</td>
    <td>&nbsp;</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap">6.02 KB</td>
    <td>8.66x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">8.57 KB</td>
    <td>12.33x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap">5.98 KB</td>
    <td>8.6x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap">41.10 KB</td>
    <td>59.11x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap">207.50 KB</td>
    <td>298.43x</td>
  </tr>
</table>