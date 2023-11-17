Benchmark

Benchmark run from 2023-11-17 20:53:14.194333Z UTC

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
    <td style="white-space: nowrap; text-align: right">11.25 K</td>
    <td style="white-space: nowrap; text-align: right">88.91 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;355.38%</td>
    <td style="white-space: nowrap; text-align: right">61.90 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">102.77 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">4.58 K</td>
    <td style="white-space: nowrap; text-align: right">218.21 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;286.22%</td>
    <td style="white-space: nowrap; text-align: right">110.75 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3527.08 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">3.77 K</td>
    <td style="white-space: nowrap; text-align: right">265.51 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;241.74%</td>
    <td style="white-space: nowrap; text-align: right">131.87 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3524.96 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">3.69 K</td>
    <td style="white-space: nowrap; text-align: right">271.08 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;247.52%</td>
    <td style="white-space: nowrap; text-align: right">122.84 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3543.27 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">2.53 K</td>
    <td style="white-space: nowrap; text-align: right">395.14 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;206.85%</td>
    <td style="white-space: nowrap; text-align: right">166.74 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3551.59 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.01 K</td>
    <td style="white-space: nowrap; text-align: right">991.74 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;126.34%</td>
    <td style="white-space: nowrap; text-align: right">487.72 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">4168.79 &micro;s</td>
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
    <td style="white-space: nowrap;text-align: right">11.25 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">4.58 K</td>
    <td style="white-space: nowrap; text-align: right">2.45x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">3.77 K</td>
    <td style="white-space: nowrap; text-align: right">2.99x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">3.69 K</td>
    <td style="white-space: nowrap; text-align: right">3.05x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">2.53 K</td>
    <td style="white-space: nowrap; text-align: right">4.44x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.01 K</td>
    <td style="white-space: nowrap; text-align: right">11.15x</td>
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
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap">89.41 KB</td>
    <td>11.13x</td>
  </tr>
    <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">99.41 KB</td>
    <td>12.38x</td>
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
    <td style="white-space: nowrap; text-align: right">1162.51</td>
    <td style="white-space: nowrap; text-align: right">0.86 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;101.71%</td>
    <td style="white-space: nowrap; text-align: right">0.66 ms</td>
    <td style="white-space: nowrap; text-align: right">4.44 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">429.48</td>
    <td style="white-space: nowrap; text-align: right">2.33 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;76.73%</td>
    <td style="white-space: nowrap; text-align: right">1.09 ms</td>
    <td style="white-space: nowrap; text-align: right">4.92 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">401.95</td>
    <td style="white-space: nowrap; text-align: right">2.49 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;72.83%</td>
    <td style="white-space: nowrap; text-align: right">1.23 ms</td>
    <td style="white-space: nowrap; text-align: right">5.31 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">328.34</td>
    <td style="white-space: nowrap; text-align: right">3.05 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;60.55%</td>
    <td style="white-space: nowrap; text-align: right">1.91 ms</td>
    <td style="white-space: nowrap; text-align: right">6.06 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">212.88</td>
    <td style="white-space: nowrap; text-align: right">4.70 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;33.25%</td>
    <td style="white-space: nowrap; text-align: right">5.32 ms</td>
    <td style="white-space: nowrap; text-align: right">8.42 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">79.56</td>
    <td style="white-space: nowrap; text-align: right">12.57 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.79%</td>
    <td style="white-space: nowrap; text-align: right">12.53 ms</td>
    <td style="white-space: nowrap; text-align: right">13.54 ms</td>
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
    <td style="white-space: nowrap;text-align: right">1162.51</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">429.48</td>
    <td style="white-space: nowrap; text-align: right">2.71x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">401.95</td>
    <td style="white-space: nowrap; text-align: right">2.89x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">328.34</td>
    <td style="white-space: nowrap; text-align: right">3.54x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">212.88</td>
    <td style="white-space: nowrap; text-align: right">5.46x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">79.56</td>
    <td style="white-space: nowrap; text-align: right">14.61x</td>
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
    <td style="white-space: nowrap; text-align: right">3.59 K</td>
    <td style="white-space: nowrap; text-align: right">278.84 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;177.25%</td>
    <td style="white-space: nowrap; text-align: right">219.08 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3906.28 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.52 K</td>
    <td style="white-space: nowrap; text-align: right">658.98 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;158.66%</td>
    <td style="white-space: nowrap; text-align: right">297.92 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3810.46 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.30 K</td>
    <td style="white-space: nowrap; text-align: right">770.76 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;138.78%</td>
    <td style="white-space: nowrap; text-align: right">403.43 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3904.24 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.19 K</td>
    <td style="white-space: nowrap; text-align: right">837.93 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;135.69%</td>
    <td style="white-space: nowrap; text-align: right">437.74 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3951.53 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">0.66 K</td>
    <td style="white-space: nowrap; text-align: right">1522.99 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;98.06%</td>
    <td style="white-space: nowrap; text-align: right">843.28 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">4452.91 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.199 K</td>
    <td style="white-space: nowrap; text-align: right">5022.67 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;3.01%</td>
    <td style="white-space: nowrap; text-align: right">5013.03 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">5595.49 &micro;s</td>
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
    <td style="white-space: nowrap;text-align: right">3.59 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.52 K</td>
    <td style="white-space: nowrap; text-align: right">2.36x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.30 K</td>
    <td style="white-space: nowrap; text-align: right">2.76x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.19 K</td>
    <td style="white-space: nowrap; text-align: right">3.01x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">0.66 K</td>
    <td style="white-space: nowrap; text-align: right">5.46x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.199 K</td>
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
    <td style="white-space: nowrap; text-align: right">51.97</td>
    <td style="white-space: nowrap; text-align: right">19.24 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;9.20%</td>
    <td style="white-space: nowrap; text-align: right">18.86 ms</td>
    <td style="white-space: nowrap; text-align: right">27.99 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">18.34</td>
    <td style="white-space: nowrap; text-align: right">54.53 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.34%</td>
    <td style="white-space: nowrap; text-align: right">54.50 ms</td>
    <td style="white-space: nowrap; text-align: right">58.43 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">17.94</td>
    <td style="white-space: nowrap; text-align: right">55.73 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;8.35%</td>
    <td style="white-space: nowrap; text-align: right">56.63 ms</td>
    <td style="white-space: nowrap; text-align: right">59.56 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">16.51</td>
    <td style="white-space: nowrap; text-align: right">60.56 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;41.91%</td>
    <td style="white-space: nowrap; text-align: right">55.27 ms</td>
    <td style="white-space: nowrap; text-align: right">126.02 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">8.54</td>
    <td style="white-space: nowrap; text-align: right">117.07 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;10.03%</td>
    <td style="white-space: nowrap; text-align: right">119.21 ms</td>
    <td style="white-space: nowrap; text-align: right">130.74 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">3.08</td>
    <td style="white-space: nowrap; text-align: right">324.84 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;0.88%</td>
    <td style="white-space: nowrap; text-align: right">325.58 ms</td>
    <td style="white-space: nowrap; text-align: right">328.27 ms</td>
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
    <td style="white-space: nowrap;text-align: right">51.97</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">18.34</td>
    <td style="white-space: nowrap; text-align: right">2.83x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">17.94</td>
    <td style="white-space: nowrap; text-align: right">2.9x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">16.51</td>
    <td style="white-space: nowrap; text-align: right">3.15x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">8.54</td>
    <td style="white-space: nowrap; text-align: right">6.08x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">3.08</td>
    <td style="white-space: nowrap; text-align: right">16.88x</td>
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
    <td style="white-space: nowrap">25.14 MB</td>
    <td>7.61x</td>
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
    <td style="white-space: nowrap; text-align: right">36.70</td>
    <td style="white-space: nowrap; text-align: right">27.25 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.67%</td>
    <td style="white-space: nowrap; text-align: right">27.17 ms</td>
    <td style="white-space: nowrap; text-align: right">28.46 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">27.42</td>
    <td style="white-space: nowrap; text-align: right">36.47 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;5.64%</td>
    <td style="white-space: nowrap; text-align: right">37.47 ms</td>
    <td style="white-space: nowrap; text-align: right">40.91 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">24.17</td>
    <td style="white-space: nowrap; text-align: right">41.37 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;4.77%</td>
    <td style="white-space: nowrap; text-align: right">42.39 ms</td>
    <td style="white-space: nowrap; text-align: right">46.19 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">16.97</td>
    <td style="white-space: nowrap; text-align: right">58.93 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;3.40%</td>
    <td style="white-space: nowrap; text-align: right">59.95 ms</td>
    <td style="white-space: nowrap; text-align: right">63.15 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">16.66</td>
    <td style="white-space: nowrap; text-align: right">60.02 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.91%</td>
    <td style="white-space: nowrap; text-align: right">60.87 ms</td>
    <td style="white-space: nowrap; text-align: right">64.38 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">8.80</td>
    <td style="white-space: nowrap; text-align: right">113.65 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.40%</td>
    <td style="white-space: nowrap; text-align: right">112.32 ms</td>
    <td style="white-space: nowrap; text-align: right">121.82 ms</td>
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
    <td style="white-space: nowrap; text-align: right">27.42</td>
    <td style="white-space: nowrap; text-align: right">1.34x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">24.17</td>
    <td style="white-space: nowrap; text-align: right">1.52x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">16.97</td>
    <td style="white-space: nowrap; text-align: right">2.16x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">16.66</td>
    <td style="white-space: nowrap; text-align: right">2.2x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">8.80</td>
    <td style="white-space: nowrap; text-align: right">4.17x</td>
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
    <td style="white-space: nowrap; text-align: right">1160.30</td>
    <td style="white-space: nowrap; text-align: right">0.86 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;93.42%</td>
    <td style="white-space: nowrap; text-align: right">0.68 ms</td>
    <td style="white-space: nowrap; text-align: right">4.58 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">410.70</td>
    <td style="white-space: nowrap; text-align: right">2.43 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;80.52%</td>
    <td style="white-space: nowrap; text-align: right">1.07 ms</td>
    <td style="white-space: nowrap; text-align: right">7.71 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">406.01</td>
    <td style="white-space: nowrap; text-align: right">2.46 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;75.06%</td>
    <td style="white-space: nowrap; text-align: right">1.25 ms</td>
    <td style="white-space: nowrap; text-align: right">7.80 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">382.58</td>
    <td style="white-space: nowrap; text-align: right">2.61 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;69.53%</td>
    <td style="white-space: nowrap; text-align: right">1.38 ms</td>
    <td style="white-space: nowrap; text-align: right">5.42 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">268.09</td>
    <td style="white-space: nowrap; text-align: right">3.73 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;49.41%</td>
    <td style="white-space: nowrap; text-align: right">4.86 ms</td>
    <td style="white-space: nowrap; text-align: right">8.17 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">97.89</td>
    <td style="white-space: nowrap; text-align: right">10.22 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;23.17%</td>
    <td style="white-space: nowrap; text-align: right">8.94 ms</td>
    <td style="white-space: nowrap; text-align: right">13.13 ms</td>
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
    <td style="white-space: nowrap;text-align: right">1160.30</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">410.70</td>
    <td style="white-space: nowrap; text-align: right">2.83x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">406.01</td>
    <td style="white-space: nowrap; text-align: right">2.86x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">382.58</td>
    <td style="white-space: nowrap; text-align: right">3.03x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">268.09</td>
    <td style="white-space: nowrap; text-align: right">4.33x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">97.89</td>
    <td style="white-space: nowrap; text-align: right">11.85x</td>
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
    <td style="white-space: nowrap; text-align: right">1665.32</td>
    <td style="white-space: nowrap; text-align: right">0.60 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;116.43%</td>
    <td style="white-space: nowrap; text-align: right">0.46 ms</td>
    <td style="white-space: nowrap; text-align: right">4.07 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">627.66</td>
    <td style="white-space: nowrap; text-align: right">1.59 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;101.72%</td>
    <td style="white-space: nowrap; text-align: right">0.71 ms</td>
    <td style="white-space: nowrap; text-align: right">4.39 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">612.18</td>
    <td style="white-space: nowrap; text-align: right">1.63 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;98.01%</td>
    <td style="white-space: nowrap; text-align: right">0.77 ms</td>
    <td style="white-space: nowrap; text-align: right">4.40 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">453.72</td>
    <td style="white-space: nowrap; text-align: right">2.20 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;84.15%</td>
    <td style="white-space: nowrap; text-align: right">1.01 ms</td>
    <td style="white-space: nowrap; text-align: right">7.63 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">320.50</td>
    <td style="white-space: nowrap; text-align: right">3.12 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;58.56%</td>
    <td style="white-space: nowrap; text-align: right">4.37 ms</td>
    <td style="white-space: nowrap; text-align: right">7.82 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">107.62</td>
    <td style="white-space: nowrap; text-align: right">9.29 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;24.59%</td>
    <td style="white-space: nowrap; text-align: right">7.65 ms</td>
    <td style="white-space: nowrap; text-align: right">13.99 ms</td>
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
    <td style="white-space: nowrap;text-align: right">1665.32</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">627.66</td>
    <td style="white-space: nowrap; text-align: right">2.65x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">612.18</td>
    <td style="white-space: nowrap; text-align: right">2.72x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">453.72</td>
    <td style="white-space: nowrap; text-align: right">3.67x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">320.50</td>
    <td style="white-space: nowrap; text-align: right">5.2x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">107.62</td>
    <td style="white-space: nowrap; text-align: right">15.47x</td>
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
    <td style="white-space: nowrap; text-align: right">14.26 K</td>
    <td style="white-space: nowrap; text-align: right">70.15 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;28.99%</td>
    <td style="white-space: nowrap; text-align: right">68.81 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">94.58 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">10.01 K</td>
    <td style="white-space: nowrap; text-align: right">99.90 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;253.47%</td>
    <td style="white-space: nowrap; text-align: right">80.69 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">119.74 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">9.33 K</td>
    <td style="white-space: nowrap; text-align: right">107.17 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;246.43%</td>
    <td style="white-space: nowrap; text-align: right">84.84 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">130.85 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">9.12 K</td>
    <td style="white-space: nowrap; text-align: right">109.63 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;228.66%</td>
    <td style="white-space: nowrap; text-align: right">90.64 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">130.01 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">4.75 K</td>
    <td style="white-space: nowrap; text-align: right">210.43 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;214.90%</td>
    <td style="white-space: nowrap; text-align: right">153.92 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3607.38 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.82 K</td>
    <td style="white-space: nowrap; text-align: right">548.28 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;171.98%</td>
    <td style="white-space: nowrap; text-align: right">265.19 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3511.18 &micro;s</td>
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
    <td style="white-space: nowrap;text-align: right">14.26 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">10.01 K</td>
    <td style="white-space: nowrap; text-align: right">1.42x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">9.33 K</td>
    <td style="white-space: nowrap; text-align: right">1.53x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">9.12 K</td>
    <td style="white-space: nowrap; text-align: right">1.56x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">4.75 K</td>
    <td style="white-space: nowrap; text-align: right">3.0x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.82 K</td>
    <td style="white-space: nowrap; text-align: right">7.82x</td>
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