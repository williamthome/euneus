Benchmark

Benchmark run from 2023-11-18 00:57:06.623275Z UTC

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
    <td style="white-space: nowrap; text-align: right">11.28 K</td>
    <td style="white-space: nowrap; text-align: right">88.69 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;352.37%</td>
    <td style="white-space: nowrap; text-align: right">61.97 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">105.02 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">4.59 K</td>
    <td style="white-space: nowrap; text-align: right">217.82 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;292.19%</td>
    <td style="white-space: nowrap; text-align: right">97.98 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3520.87 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">3.79 K</td>
    <td style="white-space: nowrap; text-align: right">263.75 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;253.92%</td>
    <td style="white-space: nowrap; text-align: right">117.76 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3524.56 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">3.79 K</td>
    <td style="white-space: nowrap; text-align: right">264.15 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;241.83%</td>
    <td style="white-space: nowrap; text-align: right">132.02 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3518.85 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">2.54 K</td>
    <td style="white-space: nowrap; text-align: right">393.56 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;211.64%</td>
    <td style="white-space: nowrap; text-align: right">164.68 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3578.85 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.01 K</td>
    <td style="white-space: nowrap; text-align: right">993.66 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;127.54%</td>
    <td style="white-space: nowrap; text-align: right">481.65 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">4226.62 &micro;s</td>
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
    <td style="white-space: nowrap;text-align: right">11.28 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">4.59 K</td>
    <td style="white-space: nowrap; text-align: right">2.46x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">3.79 K</td>
    <td style="white-space: nowrap; text-align: right">2.97x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">3.79 K</td>
    <td style="white-space: nowrap; text-align: right">2.98x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">2.54 K</td>
    <td style="white-space: nowrap; text-align: right">4.44x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.01 K</td>
    <td style="white-space: nowrap; text-align: right">11.2x</td>
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
    <td style="white-space: nowrap">99.41 KB</td>
    <td>12.38x</td>
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
    <td style="white-space: nowrap; text-align: right">1172.88</td>
    <td style="white-space: nowrap; text-align: right">0.85 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;102.97%</td>
    <td style="white-space: nowrap; text-align: right">0.65 ms</td>
    <td style="white-space: nowrap; text-align: right">4.45 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">424.49</td>
    <td style="white-space: nowrap; text-align: right">2.36 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;77.50%</td>
    <td style="white-space: nowrap; text-align: right">1.08 ms</td>
    <td style="white-space: nowrap; text-align: right">5.67 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">402.26</td>
    <td style="white-space: nowrap; text-align: right">2.49 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;73.19%</td>
    <td style="white-space: nowrap; text-align: right">1.22 ms</td>
    <td style="white-space: nowrap; text-align: right">5.27 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">332.49</td>
    <td style="white-space: nowrap; text-align: right">3.01 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;61.88%</td>
    <td style="white-space: nowrap; text-align: right">4.24 ms</td>
    <td style="white-space: nowrap; text-align: right">7.95 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">213.21</td>
    <td style="white-space: nowrap; text-align: right">4.69 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;33.16%</td>
    <td style="white-space: nowrap; text-align: right">5.31 ms</td>
    <td style="white-space: nowrap; text-align: right">8.38 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">79.30</td>
    <td style="white-space: nowrap; text-align: right">12.61 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.81%</td>
    <td style="white-space: nowrap; text-align: right">12.57 ms</td>
    <td style="white-space: nowrap; text-align: right">13.45 ms</td>
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
    <td style="white-space: nowrap;text-align: right">1172.88</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">424.49</td>
    <td style="white-space: nowrap; text-align: right">2.76x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">402.26</td>
    <td style="white-space: nowrap; text-align: right">2.92x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">332.49</td>
    <td style="white-space: nowrap; text-align: right">3.53x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">213.21</td>
    <td style="white-space: nowrap; text-align: right">5.5x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">79.30</td>
    <td style="white-space: nowrap; text-align: right">14.79x</td>
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
    <td>9.3x</td>
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
    <td style="white-space: nowrap; text-align: right">3.26 K</td>
    <td style="white-space: nowrap; text-align: right">306.95 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;159.80%</td>
    <td style="white-space: nowrap; text-align: right">218.58 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3863.54 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.52 K</td>
    <td style="white-space: nowrap; text-align: right">658.36 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;158.95%</td>
    <td style="white-space: nowrap; text-align: right">299.30 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3813.25 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.30 K</td>
    <td style="white-space: nowrap; text-align: right">769.35 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;138.35%</td>
    <td style="white-space: nowrap; text-align: right">406.24 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3910.71 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.22 K</td>
    <td style="white-space: nowrap; text-align: right">820.25 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;137.96%</td>
    <td style="white-space: nowrap; text-align: right">413.44 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3891.47 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">0.66 K</td>
    <td style="white-space: nowrap; text-align: right">1512.88 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;96.69%</td>
    <td style="white-space: nowrap; text-align: right">831.21 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">4298.67 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.196 K</td>
    <td style="white-space: nowrap; text-align: right">5101.75 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;3.15%</td>
    <td style="white-space: nowrap; text-align: right">5096.99 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">5549.02 &micro;s</td>
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
    <td style="white-space: nowrap;text-align: right">3.26 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.52 K</td>
    <td style="white-space: nowrap; text-align: right">2.14x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.30 K</td>
    <td style="white-space: nowrap; text-align: right">2.51x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.22 K</td>
    <td style="white-space: nowrap; text-align: right">2.67x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">0.66 K</td>
    <td style="white-space: nowrap; text-align: right">4.93x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.196 K</td>
    <td style="white-space: nowrap; text-align: right">16.62x</td>
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
    <td style="white-space: nowrap">294.32 KB</td>
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
    <td style="white-space: nowrap; text-align: right">52.16</td>
    <td style="white-space: nowrap; text-align: right">19.17 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;9.10%</td>
    <td style="white-space: nowrap; text-align: right">18.77 ms</td>
    <td style="white-space: nowrap; text-align: right">27.78 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">18.21</td>
    <td style="white-space: nowrap; text-align: right">54.92 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;8.43%</td>
    <td style="white-space: nowrap; text-align: right">55.79 ms</td>
    <td style="white-space: nowrap; text-align: right">61.22 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">16.95</td>
    <td style="white-space: nowrap; text-align: right">58.99 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;41.75%</td>
    <td style="white-space: nowrap; text-align: right">52.60 ms</td>
    <td style="white-space: nowrap; text-align: right">122.24 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">16.66</td>
    <td style="white-space: nowrap; text-align: right">60.01 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;25.49%</td>
    <td style="white-space: nowrap; text-align: right">62.68 ms</td>
    <td style="white-space: nowrap; text-align: right">83.78 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">8.55</td>
    <td style="white-space: nowrap; text-align: right">116.91 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;9.67%</td>
    <td style="white-space: nowrap; text-align: right">118.99 ms</td>
    <td style="white-space: nowrap; text-align: right">128.94 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">3.10</td>
    <td style="white-space: nowrap; text-align: right">322.59 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;0.83%</td>
    <td style="white-space: nowrap; text-align: right">323.88 ms</td>
    <td style="white-space: nowrap; text-align: right">325.47 ms</td>
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
    <td style="white-space: nowrap;text-align: right">52.16</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">18.21</td>
    <td style="white-space: nowrap; text-align: right">2.86x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">16.95</td>
    <td style="white-space: nowrap; text-align: right">3.08x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">16.66</td>
    <td style="white-space: nowrap; text-align: right">3.13x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">8.55</td>
    <td style="white-space: nowrap; text-align: right">6.1x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">3.10</td>
    <td style="white-space: nowrap; text-align: right">16.83x</td>
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
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">25.14 MB</td>
    <td>7.61x</td>
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
    <td style="white-space: nowrap; text-align: right">36.70</td>
    <td style="white-space: nowrap; text-align: right">27.24 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.63%</td>
    <td style="white-space: nowrap; text-align: right">27.18 ms</td>
    <td style="white-space: nowrap; text-align: right">28.66 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">27.43</td>
    <td style="white-space: nowrap; text-align: right">36.45 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;5.44%</td>
    <td style="white-space: nowrap; text-align: right">37.48 ms</td>
    <td style="white-space: nowrap; text-align: right">40.64 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">24.12</td>
    <td style="white-space: nowrap; text-align: right">41.46 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;4.99%</td>
    <td style="white-space: nowrap; text-align: right">42.35 ms</td>
    <td style="white-space: nowrap; text-align: right">45.80 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">16.98</td>
    <td style="white-space: nowrap; text-align: right">58.89 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;3.48%</td>
    <td style="white-space: nowrap; text-align: right">59.90 ms</td>
    <td style="white-space: nowrap; text-align: right">63.25 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">16.66</td>
    <td style="white-space: nowrap; text-align: right">60.04 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.66%</td>
    <td style="white-space: nowrap; text-align: right">60.87 ms</td>
    <td style="white-space: nowrap; text-align: right">61.96 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">8.78</td>
    <td style="white-space: nowrap; text-align: right">113.86 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.88%</td>
    <td style="white-space: nowrap; text-align: right">112.96 ms</td>
    <td style="white-space: nowrap; text-align: right">122.23 ms</td>
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
    <td style="white-space: nowrap;text-align: right">36.70</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">27.43</td>
    <td style="white-space: nowrap; text-align: right">1.34x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">24.12</td>
    <td style="white-space: nowrap; text-align: right">1.52x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">16.98</td>
    <td style="white-space: nowrap; text-align: right">2.16x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">16.66</td>
    <td style="white-space: nowrap; text-align: right">2.2x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">8.78</td>
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
    <td>86.54x</td>
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
    <td style="white-space: nowrap; text-align: right">1188.50</td>
    <td style="white-space: nowrap; text-align: right">0.84 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;94.90%</td>
    <td style="white-space: nowrap; text-align: right">0.68 ms</td>
    <td style="white-space: nowrap; text-align: right">4.54 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">412.20</td>
    <td style="white-space: nowrap; text-align: right">2.43 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;80.50%</td>
    <td style="white-space: nowrap; text-align: right">1.06 ms</td>
    <td style="white-space: nowrap; text-align: right">7.72 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">405.50</td>
    <td style="white-space: nowrap; text-align: right">2.47 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;74.66%</td>
    <td style="white-space: nowrap; text-align: right">1.23 ms</td>
    <td style="white-space: nowrap; text-align: right">5.66 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">397.61</td>
    <td style="white-space: nowrap; text-align: right">2.52 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;70.30%</td>
    <td style="white-space: nowrap; text-align: right">1.30 ms</td>
    <td style="white-space: nowrap; text-align: right">5.03 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">272.44</td>
    <td style="white-space: nowrap; text-align: right">3.67 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;48.45%</td>
    <td style="white-space: nowrap; text-align: right">4.81 ms</td>
    <td style="white-space: nowrap; text-align: right">8.08 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">94.44</td>
    <td style="white-space: nowrap; text-align: right">10.59 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;17.87%</td>
    <td style="white-space: nowrap; text-align: right">10.87 ms</td>
    <td style="white-space: nowrap; text-align: right">13.12 ms</td>
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
    <td style="white-space: nowrap;text-align: right">1188.50</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">412.20</td>
    <td style="white-space: nowrap; text-align: right">2.88x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">405.50</td>
    <td style="white-space: nowrap; text-align: right">2.93x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">397.61</td>
    <td style="white-space: nowrap; text-align: right">2.99x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">272.44</td>
    <td style="white-space: nowrap; text-align: right">4.36x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">94.44</td>
    <td style="white-space: nowrap; text-align: right">12.59x</td>
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
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">950.31 KB</td>
    <td>8.59x</td>
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
    <td style="white-space: nowrap; text-align: right">1648.51</td>
    <td style="white-space: nowrap; text-align: right">0.61 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;115.53%</td>
    <td style="white-space: nowrap; text-align: right">0.46 ms</td>
    <td style="white-space: nowrap; text-align: right">4.07 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">628.07</td>
    <td style="white-space: nowrap; text-align: right">1.59 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;102.45%</td>
    <td style="white-space: nowrap; text-align: right">0.70 ms</td>
    <td style="white-space: nowrap; text-align: right">4.42 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">613.82</td>
    <td style="white-space: nowrap; text-align: right">1.63 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;98.01%</td>
    <td style="white-space: nowrap; text-align: right">0.76 ms</td>
    <td style="white-space: nowrap; text-align: right">4.39 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">470.18</td>
    <td style="white-space: nowrap; text-align: right">2.13 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;84.65%</td>
    <td style="white-space: nowrap; text-align: right">0.95 ms</td>
    <td style="white-space: nowrap; text-align: right">5.64 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">321.84</td>
    <td style="white-space: nowrap; text-align: right">3.11 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;58.65%</td>
    <td style="white-space: nowrap; text-align: right">4.36 ms</td>
    <td style="white-space: nowrap; text-align: right">7.77 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">108.45</td>
    <td style="white-space: nowrap; text-align: right">9.22 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;24.11%</td>
    <td style="white-space: nowrap; text-align: right">7.65 ms</td>
    <td style="white-space: nowrap; text-align: right">13.92 ms</td>
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
    <td style="white-space: nowrap;text-align: right">1648.51</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">628.07</td>
    <td style="white-space: nowrap; text-align: right">2.62x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">613.82</td>
    <td style="white-space: nowrap; text-align: right">2.69x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">470.18</td>
    <td style="white-space: nowrap; text-align: right">3.51x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">321.84</td>
    <td style="white-space: nowrap; text-align: right">5.12x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">108.45</td>
    <td style="white-space: nowrap; text-align: right">15.2x</td>
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
    <td style="white-space: nowrap">814.30 KB</td>
    <td>15.71x</td>
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
    <td style="white-space: nowrap; text-align: right">14.38 K</td>
    <td style="white-space: nowrap; text-align: right">69.56 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;28.88%</td>
    <td style="white-space: nowrap; text-align: right">68.40 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">91.77 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">10.09 K</td>
    <td style="white-space: nowrap; text-align: right">99.10 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;251.63%</td>
    <td style="white-space: nowrap; text-align: right">80.27 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">119.44 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">9.41 K</td>
    <td style="white-space: nowrap; text-align: right">106.29 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;250.57%</td>
    <td style="white-space: nowrap; text-align: right">84.05 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">132.52 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">9.13 K</td>
    <td style="white-space: nowrap; text-align: right">109.47 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;224.01%</td>
    <td style="white-space: nowrap; text-align: right">90.72 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">128.84 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">4.76 K</td>
    <td style="white-space: nowrap; text-align: right">210.06 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;215.90%</td>
    <td style="white-space: nowrap; text-align: right">146.41 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3614.89 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.79 K</td>
    <td style="white-space: nowrap; text-align: right">557.78 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;170.99%</td>
    <td style="white-space: nowrap; text-align: right">264.75 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3565.66 &micro;s</td>
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
    <td style="white-space: nowrap;text-align: right">14.38 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">10.09 K</td>
    <td style="white-space: nowrap; text-align: right">1.42x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">9.41 K</td>
    <td style="white-space: nowrap; text-align: right">1.53x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">9.13 K</td>
    <td style="white-space: nowrap; text-align: right">1.57x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">4.76 K</td>
    <td style="white-space: nowrap; text-align: right">3.02x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.79 K</td>
    <td style="white-space: nowrap; text-align: right">8.02x</td>
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