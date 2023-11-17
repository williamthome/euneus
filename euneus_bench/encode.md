Benchmark

Benchmark run from 2023-11-17 19:15:04.057925Z UTC

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
    <td style="white-space: nowrap; text-align: right">11.14 K</td>
    <td style="white-space: nowrap; text-align: right">89.76 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;352.35%</td>
    <td style="white-space: nowrap; text-align: right">61.95 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">110.43 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">4.60 K</td>
    <td style="white-space: nowrap; text-align: right">217.17 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;290.14%</td>
    <td style="white-space: nowrap; text-align: right">91.99 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3540.90 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">4.49 K</td>
    <td style="white-space: nowrap; text-align: right">222.70 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;280.02%</td>
    <td style="white-space: nowrap; text-align: right">95.21 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3511.77 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">3.78 K</td>
    <td style="white-space: nowrap; text-align: right">264.25 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;242.74%</td>
    <td style="white-space: nowrap; text-align: right">130.87 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3521.53 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">2.54 K</td>
    <td style="white-space: nowrap; text-align: right">394.27 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;207.74%</td>
    <td style="white-space: nowrap; text-align: right">165.68 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3555.78 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.00 K</td>
    <td style="white-space: nowrap; text-align: right">1003.48 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;126.30%</td>
    <td style="white-space: nowrap; text-align: right">489.05 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">4223.15 &micro;s</td>
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
    <td style="white-space: nowrap;text-align: right">11.14 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">4.60 K</td>
    <td style="white-space: nowrap; text-align: right">2.42x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">4.49 K</td>
    <td style="white-space: nowrap; text-align: right">2.48x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">3.78 K</td>
    <td style="white-space: nowrap; text-align: right">2.94x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">2.54 K</td>
    <td style="white-space: nowrap; text-align: right">4.39x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.00 K</td>
    <td style="white-space: nowrap; text-align: right">11.18x</td>
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
    <td style="white-space: nowrap">83.10 KB</td>
    <td>10.35x</td>
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
    <td style="white-space: nowrap; text-align: right">1162.06</td>
    <td style="white-space: nowrap; text-align: right">0.86 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;103.97%</td>
    <td style="white-space: nowrap; text-align: right">0.65 ms</td>
    <td style="white-space: nowrap; text-align: right">4.54 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">429.19</td>
    <td style="white-space: nowrap; text-align: right">2.33 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;76.72%</td>
    <td style="white-space: nowrap; text-align: right">1.08 ms</td>
    <td style="white-space: nowrap; text-align: right">4.96 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">427.23</td>
    <td style="white-space: nowrap; text-align: right">2.34 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;76.28%</td>
    <td style="white-space: nowrap; text-align: right">1.10 ms</td>
    <td style="white-space: nowrap; text-align: right">4.87 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">403.55</td>
    <td style="white-space: nowrap; text-align: right">2.48 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;73.07%</td>
    <td style="white-space: nowrap; text-align: right">1.22 ms</td>
    <td style="white-space: nowrap; text-align: right">5.60 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">213.42</td>
    <td style="white-space: nowrap; text-align: right">4.69 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;33.18%</td>
    <td style="white-space: nowrap; text-align: right">5.32 ms</td>
    <td style="white-space: nowrap; text-align: right">8.39 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">78.73</td>
    <td style="white-space: nowrap; text-align: right">12.70 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.92%</td>
    <td style="white-space: nowrap; text-align: right">12.65 ms</td>
    <td style="white-space: nowrap; text-align: right">13.74 ms</td>
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
    <td style="white-space: nowrap;text-align: right">1162.06</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">429.19</td>
    <td style="white-space: nowrap; text-align: right">2.71x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">427.23</td>
    <td style="white-space: nowrap; text-align: right">2.72x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">403.55</td>
    <td style="white-space: nowrap; text-align: right">2.88x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">213.42</td>
    <td style="white-space: nowrap; text-align: right">5.44x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">78.73</td>
    <td style="white-space: nowrap; text-align: right">14.76x</td>
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
    <td style="white-space: nowrap">116.75 KB</td>
    <td>&nbsp;</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap">861.52 KB</td>
    <td>7.38x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">870.26 KB</td>
    <td>7.45x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap">861.35 KB</td>
    <td>7.38x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap">2239.20 KB</td>
    <td>19.18x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap">4925.84 KB</td>
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
    <td style="white-space: nowrap; text-align: right">3.55 K</td>
    <td style="white-space: nowrap; text-align: right">281.69 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;174.87%</td>
    <td style="white-space: nowrap; text-align: right">205.36 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3861.87 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.51 K</td>
    <td style="white-space: nowrap; text-align: right">660.74 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;158.73%</td>
    <td style="white-space: nowrap; text-align: right">300.66 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3824.97 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.45 K</td>
    <td style="white-space: nowrap; text-align: right">688.66 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;153.49%</td>
    <td style="white-space: nowrap; text-align: right">315.02 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3814.31 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.30 K</td>
    <td style="white-space: nowrap; text-align: right">767.20 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;139.02%</td>
    <td style="white-space: nowrap; text-align: right">401.38 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3913.49 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">0.67 K</td>
    <td style="white-space: nowrap; text-align: right">1503.22 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;97.77%</td>
    <td style="white-space: nowrap; text-align: right">836.18 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">4390.12 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.197 K</td>
    <td style="white-space: nowrap; text-align: right">5073.06 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;3.09%</td>
    <td style="white-space: nowrap; text-align: right">5059.26 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">5549.29 &micro;s</td>
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
    <td style="white-space: nowrap;text-align: right">3.55 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.51 K</td>
    <td style="white-space: nowrap; text-align: right">2.35x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.45 K</td>
    <td style="white-space: nowrap; text-align: right">2.44x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.30 K</td>
    <td style="white-space: nowrap; text-align: right">2.72x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">0.67 K</td>
    <td style="white-space: nowrap; text-align: right">5.34x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.197 K</td>
    <td style="white-space: nowrap; text-align: right">18.01x</td>
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
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">228.73 KB</td>
    <td>5.34x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap">228.15 KB</td>
    <td>5.32x</td>
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
    <td style="white-space: nowrap; text-align: right">52.04</td>
    <td style="white-space: nowrap; text-align: right">19.22 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;9.29%</td>
    <td style="white-space: nowrap; text-align: right">18.80 ms</td>
    <td style="white-space: nowrap; text-align: right">28.39 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">17.91</td>
    <td style="white-space: nowrap; text-align: right">55.83 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;8.32%</td>
    <td style="white-space: nowrap; text-align: right">56.71 ms</td>
    <td style="white-space: nowrap; text-align: right">62.45 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">16.56</td>
    <td style="white-space: nowrap; text-align: right">60.38 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;42.18%</td>
    <td style="white-space: nowrap; text-align: right">55.65 ms</td>
    <td style="white-space: nowrap; text-align: right">126.02 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">14.72</td>
    <td style="white-space: nowrap; text-align: right">67.93 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;45.22%</td>
    <td style="white-space: nowrap; text-align: right">52.28 ms</td>
    <td style="white-space: nowrap; text-align: right">112.10 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">8.58</td>
    <td style="white-space: nowrap; text-align: right">116.48 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;9.94%</td>
    <td style="white-space: nowrap; text-align: right">118.55 ms</td>
    <td style="white-space: nowrap; text-align: right">130.28 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">3.07</td>
    <td style="white-space: nowrap; text-align: right">325.64 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;0.89%</td>
    <td style="white-space: nowrap; text-align: right">326.09 ms</td>
    <td style="white-space: nowrap; text-align: right">329.27 ms</td>
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
    <td style="white-space: nowrap;text-align: right">52.04</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">17.91</td>
    <td style="white-space: nowrap; text-align: right">2.91x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">16.56</td>
    <td style="white-space: nowrap; text-align: right">3.14x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">14.72</td>
    <td style="white-space: nowrap; text-align: right">3.53x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">8.58</td>
    <td style="white-space: nowrap; text-align: right">6.06x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">3.07</td>
    <td style="white-space: nowrap; text-align: right">16.95x</td>
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
    <td style="white-space: nowrap">20.38 MB</td>
    <td>6.17x</td>
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
    <td style="white-space: nowrap; text-align: right">36.73</td>
    <td style="white-space: nowrap; text-align: right">27.22 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.72%</td>
    <td style="white-space: nowrap; text-align: right">27.14 ms</td>
    <td style="white-space: nowrap; text-align: right">28.45 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">27.44</td>
    <td style="white-space: nowrap; text-align: right">36.44 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;5.48%</td>
    <td style="white-space: nowrap; text-align: right">37.46 ms</td>
    <td style="white-space: nowrap; text-align: right">40.95 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">23.24</td>
    <td style="white-space: nowrap; text-align: right">43.03 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;4.81%</td>
    <td style="white-space: nowrap; text-align: right">43.91 ms</td>
    <td style="white-space: nowrap; text-align: right">47.47 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">17.01</td>
    <td style="white-space: nowrap; text-align: right">58.78 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;3.50%</td>
    <td style="white-space: nowrap; text-align: right">59.77 ms</td>
    <td style="white-space: nowrap; text-align: right">64.03 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">16.65</td>
    <td style="white-space: nowrap; text-align: right">60.07 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;3.14%</td>
    <td style="white-space: nowrap; text-align: right">60.77 ms</td>
    <td style="white-space: nowrap; text-align: right">68.42 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">8.75</td>
    <td style="white-space: nowrap; text-align: right">114.31 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.24%</td>
    <td style="white-space: nowrap; text-align: right">113.12 ms</td>
    <td style="white-space: nowrap; text-align: right">120.59 ms</td>
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
    <td style="white-space: nowrap;text-align: right">36.73</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">27.44</td>
    <td style="white-space: nowrap; text-align: right">1.34x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">23.24</td>
    <td style="white-space: nowrap; text-align: right">1.58x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">17.01</td>
    <td style="white-space: nowrap; text-align: right">2.16x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">16.65</td>
    <td style="white-space: nowrap; text-align: right">2.21x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">8.75</td>
    <td style="white-space: nowrap; text-align: right">4.2x</td>
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
    <td style="white-space: nowrap">1.07 MB</td>
    <td>85.82x</td>
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
    <td style="white-space: nowrap; text-align: right">1191.94</td>
    <td style="white-space: nowrap; text-align: right">0.84 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;95.09%</td>
    <td style="white-space: nowrap; text-align: right">0.68 ms</td>
    <td style="white-space: nowrap; text-align: right">4.52 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">417.78</td>
    <td style="white-space: nowrap; text-align: right">2.39 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;78.22%</td>
    <td style="white-space: nowrap; text-align: right">1.08 ms</td>
    <td style="white-space: nowrap; text-align: right">5.07 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">411.69</td>
    <td style="white-space: nowrap; text-align: right">2.43 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;80.45%</td>
    <td style="white-space: nowrap; text-align: right">1.06 ms</td>
    <td style="white-space: nowrap; text-align: right">7.75 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">405.74</td>
    <td style="white-space: nowrap; text-align: right">2.46 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;75.14%</td>
    <td style="white-space: nowrap; text-align: right">1.23 ms</td>
    <td style="white-space: nowrap; text-align: right">7.82 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">273.00</td>
    <td style="white-space: nowrap; text-align: right">3.66 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;48.63%</td>
    <td style="white-space: nowrap; text-align: right">4.79 ms</td>
    <td style="white-space: nowrap; text-align: right">8.06 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">97.47</td>
    <td style="white-space: nowrap; text-align: right">10.26 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;22.99%</td>
    <td style="white-space: nowrap; text-align: right">9.01 ms</td>
    <td style="white-space: nowrap; text-align: right">13.15 ms</td>
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
    <td style="white-space: nowrap;text-align: right">1191.94</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">417.78</td>
    <td style="white-space: nowrap; text-align: right">2.85x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">411.69</td>
    <td style="white-space: nowrap; text-align: right">2.9x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">405.74</td>
    <td style="white-space: nowrap; text-align: right">2.94x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">273.00</td>
    <td style="white-space: nowrap; text-align: right">4.37x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">97.47</td>
    <td style="white-space: nowrap; text-align: right">12.23x</td>
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
    <td style="white-space: nowrap">785.96 KB</td>
    <td>7.1x</td>
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
    <td style="white-space: nowrap; text-align: right">1669.42</td>
    <td style="white-space: nowrap; text-align: right">0.60 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;116.99%</td>
    <td style="white-space: nowrap; text-align: right">0.47 ms</td>
    <td style="white-space: nowrap; text-align: right">4.06 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">644.19</td>
    <td style="white-space: nowrap; text-align: right">1.55 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;102.47%</td>
    <td style="white-space: nowrap; text-align: right">0.69 ms</td>
    <td style="white-space: nowrap; text-align: right">4.29 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">628.42</td>
    <td style="white-space: nowrap; text-align: right">1.59 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;102.05%</td>
    <td style="white-space: nowrap; text-align: right">0.70 ms</td>
    <td style="white-space: nowrap; text-align: right">4.43 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">613.91</td>
    <td style="white-space: nowrap; text-align: right">1.63 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;98.07%</td>
    <td style="white-space: nowrap; text-align: right">0.76 ms</td>
    <td style="white-space: nowrap; text-align: right">4.37 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">322.22</td>
    <td style="white-space: nowrap; text-align: right">3.10 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;58.66%</td>
    <td style="white-space: nowrap; text-align: right">4.35 ms</td>
    <td style="white-space: nowrap; text-align: right">7.79 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">108.10</td>
    <td style="white-space: nowrap; text-align: right">9.25 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;24.83%</td>
    <td style="white-space: nowrap; text-align: right">7.70 ms</td>
    <td style="white-space: nowrap; text-align: right">14.09 ms</td>
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
    <td style="white-space: nowrap;text-align: right">1669.42</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">644.19</td>
    <td style="white-space: nowrap; text-align: right">2.59x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">628.42</td>
    <td style="white-space: nowrap; text-align: right">2.66x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">613.91</td>
    <td style="white-space: nowrap; text-align: right">2.72x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">322.22</td>
    <td style="white-space: nowrap; text-align: right">5.18x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">108.10</td>
    <td style="white-space: nowrap; text-align: right">15.44x</td>
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
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">673.63 KB</td>
    <td>13.0x</td>
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
    <td style="white-space: nowrap; text-align: right">14.32 K</td>
    <td style="white-space: nowrap; text-align: right">69.83 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;28.81%</td>
    <td style="white-space: nowrap; text-align: right">68.64 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">91.90 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">10.11 K</td>
    <td style="white-space: nowrap; text-align: right">98.96 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;254.09%</td>
    <td style="white-space: nowrap; text-align: right">79.95 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">122.00 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">9.40 K</td>
    <td style="white-space: nowrap; text-align: right">106.34 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;250.48%</td>
    <td style="white-space: nowrap; text-align: right">85.14 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">128.50 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">9.07 K</td>
    <td style="white-space: nowrap; text-align: right">110.23 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;225.55%</td>
    <td style="white-space: nowrap; text-align: right">90.78 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">154.49 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">4.76 K</td>
    <td style="white-space: nowrap; text-align: right">210.18 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;214.24%</td>
    <td style="white-space: nowrap; text-align: right">154.87 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3592.07 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.83 K</td>
    <td style="white-space: nowrap; text-align: right">547.04 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;172.30%</td>
    <td style="white-space: nowrap; text-align: right">264.10 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3501.48 &micro;s</td>
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
    <td style="white-space: nowrap;text-align: right">14.32 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">10.11 K</td>
    <td style="white-space: nowrap; text-align: right">1.42x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">9.40 K</td>
    <td style="white-space: nowrap; text-align: right">1.52x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">9.07 K</td>
    <td style="white-space: nowrap; text-align: right">1.58x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">4.76 K</td>
    <td style="white-space: nowrap; text-align: right">3.01x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.83 K</td>
    <td style="white-space: nowrap; text-align: right">7.83x</td>
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
    <td style="white-space: nowrap">8.53 KB</td>
    <td>12.27x</td>
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