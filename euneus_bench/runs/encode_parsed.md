Benchmark

Benchmark run from 2023-11-17 19:50:00.373985Z UTC

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
    <td style="white-space: nowrap; text-align: right">11.13 K</td>
    <td style="white-space: nowrap; text-align: right">89.81 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;361.83%</td>
    <td style="white-space: nowrap; text-align: right">62.18 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">124.60 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">4.62 K</td>
    <td style="white-space: nowrap; text-align: right">216.52 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;289.12%</td>
    <td style="white-space: nowrap; text-align: right">91.70 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3534.56 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">3.92 K</td>
    <td style="white-space: nowrap; text-align: right">255.20 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;262.72%</td>
    <td style="white-space: nowrap; text-align: right">114.06 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3540.53 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">3.74 K</td>
    <td style="white-space: nowrap; text-align: right">267.26 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;240.54%</td>
    <td style="white-space: nowrap; text-align: right">132.57 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3535.63 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">2.55 K</td>
    <td style="white-space: nowrap; text-align: right">392.21 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;210.30%</td>
    <td style="white-space: nowrap; text-align: right">164.40 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3576.40 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.01 K</td>
    <td style="white-space: nowrap; text-align: right">990.32 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;126.13%</td>
    <td style="white-space: nowrap; text-align: right">487.72 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">4136.89 &micro;s</td>
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
    <td style="white-space: nowrap;text-align: right">11.13 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">4.62 K</td>
    <td style="white-space: nowrap; text-align: right">2.41x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">3.92 K</td>
    <td style="white-space: nowrap; text-align: right">2.84x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">3.74 K</td>
    <td style="white-space: nowrap; text-align: right">2.98x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">2.55 K</td>
    <td style="white-space: nowrap; text-align: right">4.37x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">1.01 K</td>
    <td style="white-space: nowrap; text-align: right">11.03x</td>
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
    <td style="white-space: nowrap; text-align: right">1163.05</td>
    <td style="white-space: nowrap; text-align: right">0.86 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;102.84%</td>
    <td style="white-space: nowrap; text-align: right">0.65 ms</td>
    <td style="white-space: nowrap; text-align: right">4.48 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">416.91</td>
    <td style="white-space: nowrap; text-align: right">2.40 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;77.69%</td>
    <td style="white-space: nowrap; text-align: right">1.12 ms</td>
    <td style="white-space: nowrap; text-align: right">5.43 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">396.88</td>
    <td style="white-space: nowrap; text-align: right">2.52 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;73.33%</td>
    <td style="white-space: nowrap; text-align: right">1.24 ms</td>
    <td style="white-space: nowrap; text-align: right">5.74 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">335.54</td>
    <td style="white-space: nowrap; text-align: right">2.98 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;59.79%</td>
    <td style="white-space: nowrap; text-align: right">4.21 ms</td>
    <td style="white-space: nowrap; text-align: right">5.20 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">212.68</td>
    <td style="white-space: nowrap; text-align: right">4.70 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;33.48%</td>
    <td style="white-space: nowrap; text-align: right">5.32 ms</td>
    <td style="white-space: nowrap; text-align: right">8.48 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">79.30</td>
    <td style="white-space: nowrap; text-align: right">12.61 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.20%</td>
    <td style="white-space: nowrap; text-align: right">12.55 ms</td>
    <td style="white-space: nowrap; text-align: right">13.58 ms</td>
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
    <td style="white-space: nowrap;text-align: right">1163.05</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">416.91</td>
    <td style="white-space: nowrap; text-align: right">2.79x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">396.88</td>
    <td style="white-space: nowrap; text-align: right">2.93x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">335.54</td>
    <td style="white-space: nowrap; text-align: right">3.47x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">212.68</td>
    <td style="white-space: nowrap; text-align: right">5.47x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">79.30</td>
    <td style="white-space: nowrap; text-align: right">14.67x</td>
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
    <td style="white-space: nowrap; text-align: right">3.50 K</td>
    <td style="white-space: nowrap; text-align: right">285.54 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;176.11%</td>
    <td style="white-space: nowrap; text-align: right">219.91 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3964.76 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.50 K</td>
    <td style="white-space: nowrap; text-align: right">668.16 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;157.81%</td>
    <td style="white-space: nowrap; text-align: right">317.58 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3851.53 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.28 K</td>
    <td style="white-space: nowrap; text-align: right">783.06 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;137.01%</td>
    <td style="white-space: nowrap; text-align: right">433.47 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3924.85 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.25 K</td>
    <td style="white-space: nowrap; text-align: right">802.24 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;140.27%</td>
    <td style="white-space: nowrap; text-align: right">392.98 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3853.63 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">0.66 K</td>
    <td style="white-space: nowrap; text-align: right">1517.50 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;96.50%</td>
    <td style="white-space: nowrap; text-align: right">836.07 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">4323.29 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.199 K</td>
    <td style="white-space: nowrap; text-align: right">5034.32 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;3.24%</td>
    <td style="white-space: nowrap; text-align: right">5020.78 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">5659.80 &micro;s</td>
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
    <td style="white-space: nowrap;text-align: right">3.50 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">1.50 K</td>
    <td style="white-space: nowrap; text-align: right">2.34x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">1.28 K</td>
    <td style="white-space: nowrap; text-align: right">2.74x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">1.25 K</td>
    <td style="white-space: nowrap; text-align: right">2.81x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">0.66 K</td>
    <td style="white-space: nowrap; text-align: right">5.31x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">0.199 K</td>
    <td style="white-space: nowrap; text-align: right">17.63x</td>
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
    <td style="white-space: nowrap; text-align: right">&plusmn;8.64%</td>
    <td style="white-space: nowrap; text-align: right">18.87 ms</td>
    <td style="white-space: nowrap; text-align: right">27.59 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">17.96</td>
    <td style="white-space: nowrap; text-align: right">55.68 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;8.27%</td>
    <td style="white-space: nowrap; text-align: right">56.57 ms</td>
    <td style="white-space: nowrap; text-align: right">59.50 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">17.40</td>
    <td style="white-space: nowrap; text-align: right">57.46 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;26.81%</td>
    <td style="white-space: nowrap; text-align: right">59.31 ms</td>
    <td style="white-space: nowrap; text-align: right">79.62 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">16.46</td>
    <td style="white-space: nowrap; text-align: right">60.76 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;42.27%</td>
    <td style="white-space: nowrap; text-align: right">55.43 ms</td>
    <td style="white-space: nowrap; text-align: right">127.13 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">8.54</td>
    <td style="white-space: nowrap; text-align: right">117.15 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;9.69%</td>
    <td style="white-space: nowrap; text-align: right">119.52 ms</td>
    <td style="white-space: nowrap; text-align: right">127.23 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">3.08</td>
    <td style="white-space: nowrap; text-align: right">325.09 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;0.91%</td>
    <td style="white-space: nowrap; text-align: right">325.64 ms</td>
    <td style="white-space: nowrap; text-align: right">329.13 ms</td>
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
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">17.96</td>
    <td style="white-space: nowrap; text-align: right">2.89x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">17.40</td>
    <td style="white-space: nowrap; text-align: right">2.99x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">16.46</td>
    <td style="white-space: nowrap; text-align: right">3.16x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">8.54</td>
    <td style="white-space: nowrap; text-align: right">6.09x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">3.08</td>
    <td style="white-space: nowrap; text-align: right">16.9x</td>
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
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap">25.14 MB</td>
    <td>7.61x</td>
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
    <td style="white-space: nowrap; text-align: right">36.68</td>
    <td style="white-space: nowrap; text-align: right">27.27 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;1.74%</td>
    <td style="white-space: nowrap; text-align: right">27.17 ms</td>
    <td style="white-space: nowrap; text-align: right">28.56 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">27.35</td>
    <td style="white-space: nowrap; text-align: right">36.56 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;5.59%</td>
    <td style="white-space: nowrap; text-align: right">37.58 ms</td>
    <td style="white-space: nowrap; text-align: right">40.73 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">24.04</td>
    <td style="white-space: nowrap; text-align: right">41.60 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;5.38%</td>
    <td style="white-space: nowrap; text-align: right">42.51 ms</td>
    <td style="white-space: nowrap; text-align: right">45.76 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">16.94</td>
    <td style="white-space: nowrap; text-align: right">59.04 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;3.53%</td>
    <td style="white-space: nowrap; text-align: right">59.99 ms</td>
    <td style="white-space: nowrap; text-align: right">63.80 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">16.68</td>
    <td style="white-space: nowrap; text-align: right">59.96 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.86%</td>
    <td style="white-space: nowrap; text-align: right">60.82 ms</td>
    <td style="white-space: nowrap; text-align: right">62.49 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">8.76</td>
    <td style="white-space: nowrap; text-align: right">114.15 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;2.07%</td>
    <td style="white-space: nowrap; text-align: right">114.80 ms</td>
    <td style="white-space: nowrap; text-align: right">120.18 ms</td>
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
    <td style="white-space: nowrap;text-align: right">36.68</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">27.35</td>
    <td style="white-space: nowrap; text-align: right">1.34x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">24.04</td>
    <td style="white-space: nowrap; text-align: right">1.53x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">16.94</td>
    <td style="white-space: nowrap; text-align: right">2.17x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">16.68</td>
    <td style="white-space: nowrap; text-align: right">2.2x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">8.76</td>
    <td style="white-space: nowrap; text-align: right">4.19x</td>
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
    <td style="white-space: nowrap; text-align: right">1177.53</td>
    <td style="white-space: nowrap; text-align: right">0.85 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;93.45%</td>
    <td style="white-space: nowrap; text-align: right">0.68 ms</td>
    <td style="white-space: nowrap; text-align: right">4.52 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">409.80</td>
    <td style="white-space: nowrap; text-align: right">2.44 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;73.17%</td>
    <td style="white-space: nowrap; text-align: right">1.24 ms</td>
    <td style="white-space: nowrap; text-align: right">5.00 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">406.64</td>
    <td style="white-space: nowrap; text-align: right">2.46 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;79.58%</td>
    <td style="white-space: nowrap; text-align: right">1.14 ms</td>
    <td style="white-space: nowrap; text-align: right">7.80 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">399.93</td>
    <td style="white-space: nowrap; text-align: right">2.50 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;72.96%</td>
    <td style="white-space: nowrap; text-align: right">1.41 ms</td>
    <td style="white-space: nowrap; text-align: right">5.91 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">270.72</td>
    <td style="white-space: nowrap; text-align: right">3.69 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;49.08%</td>
    <td style="white-space: nowrap; text-align: right">4.83 ms</td>
    <td style="white-space: nowrap; text-align: right">8.16 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">97.89</td>
    <td style="white-space: nowrap; text-align: right">10.22 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;23.13%</td>
    <td style="white-space: nowrap; text-align: right">8.96 ms</td>
    <td style="white-space: nowrap; text-align: right">13.09 ms</td>
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
    <td style="white-space: nowrap;text-align: right">1177.53</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">409.80</td>
    <td style="white-space: nowrap; text-align: right">2.87x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">406.64</td>
    <td style="white-space: nowrap; text-align: right">2.9x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">399.93</td>
    <td style="white-space: nowrap; text-align: right">2.94x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">270.72</td>
    <td style="white-space: nowrap; text-align: right">4.35x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">97.89</td>
    <td style="white-space: nowrap; text-align: right">12.03x</td>
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
    <td style="white-space: nowrap">950.31 KB</td>
    <td>8.59x</td>
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
    <td style="white-space: nowrap; text-align: right">1659.13</td>
    <td style="white-space: nowrap; text-align: right">0.60 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;115.51%</td>
    <td style="white-space: nowrap; text-align: right">0.46 ms</td>
    <td style="white-space: nowrap; text-align: right">4.06 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">620.01</td>
    <td style="white-space: nowrap; text-align: right">1.61 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;101.50%</td>
    <td style="white-space: nowrap; text-align: right">0.72 ms</td>
    <td style="white-space: nowrap; text-align: right">4.44 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">609.24</td>
    <td style="white-space: nowrap; text-align: right">1.64 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;98.03%</td>
    <td style="white-space: nowrap; text-align: right">0.76 ms</td>
    <td style="white-space: nowrap; text-align: right">4.43 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">487.64</td>
    <td style="white-space: nowrap; text-align: right">2.05 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;87.73%</td>
    <td style="white-space: nowrap; text-align: right">0.88 ms</td>
    <td style="white-space: nowrap; text-align: right">4.95 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">321.06</td>
    <td style="white-space: nowrap; text-align: right">3.11 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;58.68%</td>
    <td style="white-space: nowrap; text-align: right">4.38 ms</td>
    <td style="white-space: nowrap; text-align: right">7.86 ms</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">108.35</td>
    <td style="white-space: nowrap; text-align: right">9.23 ms</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;24.26%</td>
    <td style="white-space: nowrap; text-align: right">7.63 ms</td>
    <td style="white-space: nowrap; text-align: right">13.95 ms</td>
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
    <td style="white-space: nowrap;text-align: right">1659.13</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">620.01</td>
    <td style="white-space: nowrap; text-align: right">2.68x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">609.24</td>
    <td style="white-space: nowrap; text-align: right">2.72x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">487.64</td>
    <td style="white-space: nowrap; text-align: right">3.4x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">321.06</td>
    <td style="white-space: nowrap; text-align: right">5.17x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">108.35</td>
    <td style="white-space: nowrap; text-align: right">15.31x</td>
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
    <td style="white-space: nowrap; text-align: right">14.36 K</td>
    <td style="white-space: nowrap; text-align: right">69.65 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;29.78%</td>
    <td style="white-space: nowrap; text-align: right">68.51 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">91.13 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">10.07 K</td>
    <td style="white-space: nowrap; text-align: right">99.35 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;254.82%</td>
    <td style="white-space: nowrap; text-align: right">79.68 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">129.89 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">9.27 K</td>
    <td style="white-space: nowrap; text-align: right">107.88 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;248.81%</td>
    <td style="white-space: nowrap; text-align: right">84.77 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">138.31 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">9.07 K</td>
    <td style="white-space: nowrap; text-align: right">110.31 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;223.58%</td>
    <td style="white-space: nowrap; text-align: right">91.11 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">136.66 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">4.78 K</td>
    <td style="white-space: nowrap; text-align: right">209.27 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;215.25%</td>
    <td style="white-space: nowrap; text-align: right">152.76 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3594.46 &micro;s</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.83 K</td>
    <td style="white-space: nowrap; text-align: right">547.12 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">&plusmn;171.80%</td>
    <td style="white-space: nowrap; text-align: right">264.96 &micro;s</td>
    <td style="white-space: nowrap; text-align: right">3503.86 &micro;s</td>
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
    <td style="white-space: nowrap;text-align: right">14.36 K</td>
    <td>&nbsp;</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">Jason</td>
    <td style="white-space: nowrap; text-align: right">10.07 K</td>
    <td style="white-space: nowrap; text-align: right">1.43x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">euneus</td>
    <td style="white-space: nowrap; text-align: right">9.27 K</td>
    <td style="white-space: nowrap; text-align: right">1.55x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">thoas</td>
    <td style="white-space: nowrap; text-align: right">9.07 K</td>
    <td style="white-space: nowrap; text-align: right">1.58x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsx</td>
    <td style="white-space: nowrap; text-align: right">4.78 K</td>
    <td style="white-space: nowrap; text-align: right">3.0x</td>
  </tr>

  <tr>
    <td style="white-space: nowrap">jsone</td>
    <td style="white-space: nowrap; text-align: right">1.83 K</td>
    <td style="white-space: nowrap; text-align: right">7.86x</td>
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