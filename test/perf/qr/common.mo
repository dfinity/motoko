/**
 * Module     : common.mo
 * Copyright  : 2020 DFINITY Stiftung
 * License    : Apache 2.0 with LLVM Exception
 * Maintainer : Enzo Haussecker <enzo@dfinity.org>
 * Stability  : stable
 */

import Array "array";
import Galois "galois";
import List "list";
import Prelude "prelude";
import Version "version";

module {

  type List<T> = List.List<T>;
  type Poly = Galois.Poly;
  type Version = Version.Version;

  public type ErrorCorrection = { #L; #M; #Q; #H };
  public type Matrix = { #Matrix : [[Bool]] };
  public type Mode = { #Alphanumeric; #EightBit; #Kanji; #Numeric };

  public func alignments(version : Version) : [Nat] {
    [
      [],
      [6,18],
      [6,22],
      [6,26],
      [6,30],
      [6,34],
      [6,22,38],
      [6,24,42],
      [6,26,46],
      [6,28,50],
      [6,30,54],
      [6,32,58],
      [6,34,62],
      [6,26,46,66],
      [6,26,48,70],
      [6,26,50,74],
      [6,30,54,78],
      [6,30,56,82],
      [6,30,58,86],
      [6,34,62,90],
      [6,28,50,72,94],
      [6,26,50,74,98],
      [6,30,54,78,102],
      [6,28,54,80,106],
      [6,32,58,84,110],
      [6,30,58,86,114],
      [6,34,62,90,118],
      [6,26,50,74,98,122],
      [6,30,54,78,102,126],
      [6,26,52,78,104,130],
      [6,30,56,82,108,134],
      [6,34,60,86,112,138],
      [6,30,58,86,114,142],
      [6,34,62,90,118,146],
      [6,30,54,78,102,126,150],
      [6,24,50,76,102,128,154],
      [6,28,54,80,106,132,158],
      [6,32,58,84,110,136,162],
      [6,26,54,82,110,138,166]
    ][Version.unbox(version) - 1]
  };

  public func remainder(version : Version) : Nat {
    [
      0, 7, 7, 7, 7, 7, 0, 0, 0, 0,
      0, 0, 0, 3, 3, 3, 3, 3, 3, 3,
      4, 4, 4, 4, 4, 4, 4, 3, 3, 3,
      3, 3, 3, 3, 0, 0, 0, 0, 0, 0
    ][Version.unbox(version) - 1]
  };

  public func width(version : Version) : Nat {
    [
      021, 025, 029, 033, 037, 041, 045, 049, 053, 057,
      061, 065, 069, 073, 077, 081, 085, 089, 093, 097,
      101, 105, 109, 113, 117, 121, 125, 129, 133, 137,
      141, 145, 149, 153, 157, 161, 165, 169, 173, 177
    ][Version.unbox(version) - 1]
  };

  func match<X>(
    version : Version,
    level : ErrorCorrection,
    table : [X]
  ) : X {
    let i : Nat = Version.unbox(version) - 1;
    let j = switch level {
      case (#L) 0;
      case (#M) 1;
      case (#Q) 2;
      case (#H) 3
    };
    table[4 * i + j]
  };

  public func targetSize(version : Version, level : ErrorCorrection) : Nat {
    let table = [
      00152, 00128, 00104, 00072,
      00272, 00224, 00176, 00128,
      00440, 00352, 00272, 00208,
      00640, 00512, 00384, 00288,
      00864, 00688, 00496, 00368,
      01088, 00864, 00608, 00480,
      01248, 00992, 00704, 00528,
      01552, 01232, 00880, 00688,
      01856, 01456, 01056, 00800,
      02192, 01728, 01232, 00976,
      02592, 02032, 01440, 01120,
      02960, 02320, 01648, 01264,
      03424, 02672, 01952, 01440,
      03688, 02920, 02088, 01576,
      04184, 03320, 02360, 01784,
      04712, 03624, 02600, 02024,
      05176, 04056, 02936, 02264,
      05768, 04504, 03176, 02504,
      06360, 05016, 03560, 02728,
      06888, 05352, 03880, 03080,
      07456, 05712, 04096, 03248,
      08048, 06256, 04544, 03536,
      08752, 06880, 04912, 03712,
      09392, 07312, 05312, 04112,
      10208, 08000, 05744, 04304,
      10960, 08496, 06032, 04768,
      11744, 09024, 06464, 05024,
      12248, 09544, 06968, 05288,
      13048, 10136, 07288, 05608,
      13880, 10984, 07880, 05960,
      14744, 11640, 08264, 06344,
      15640, 12328, 08920, 06760,
      16568, 13048, 09368, 07208,
      17528, 13800, 09848, 07688,
      18448, 14496, 10288, 07888,
      19472, 15312, 10832, 08432,
      20528, 15936, 11408, 08768,
      21616, 16816, 12016, 09136,
      22496, 17728, 12656, 09776,
      23648, 18672, 13328, 10208
    ];
    match<Nat>(version, level, table)
  };

  public func blockSizes(version : Version, level : ErrorCorrection) : [Nat] {
    let table = [
      [19], [16], [13], [9],
      [34], [28], [22], [16],
      [55], [44], [17,17], [13,13],
      [80], [32,32], [24,24], [9,9,9,9],
      [108], [43,43], [15,15,16,16], [11,11,12,12],
      [68,68], [27,27,27,27], [19,19,19,19], [15,15,15,15],
      [78,78], [31,31,31,31], [14,14,15,15,15,15], [13,13,13,13,14],
      [97,97], [38,38,39,39], [18,18,18,18,19,19], [14,14,14,14,15,15],
      [116,116], [36,36,36,37,37], [16,16,16,16,17,17,17,17], [12,12,12,12,13,13,13,13],
      [68,68,69,69], [43,43,43,43,44], [19,19,19,19,19,19,20,20], [15,15,15,15,15,15,16,16],
      [81,81,81,81], [50,51,51,51,51], [22,22,22,22,23,23,23,23], [12,12,12,13,13,13,13,13,13,13,13],
      [92,92,93,93], [36,36,36,36,36,36,37,37], [20,20,20,20,21,21,21,21,21,21], [14,14,14,14,14,14,14,15,15,15,15],
      [107,107,107,107], [37,37,37,37,37,37,37,37,38], [20,20,20,20,20,20,20,20,21,21,21,21], [11,11,11,11,11,11,11,11,11,11,11,11,12,12,12,12],
      [115,115,115,116], [40,40,40,40,41,41,41,41,41], [16,16,16,16,16,16,16,16,16,16,16,17,17,17,17,17], [12,12,12,12,12,12,12,12,12,12,12,13,13,13,13,13],
      [87,87,87,87,87,88], [41,41,41,41,41,42,42,42,42,42], [24,24,24,24,24,25,25,25,25,25,25,25], [12,12,12,12,12,12,12,12,12,12,12,13,13,13,13,13,13,13],
      [98,98,98,98,98,99], [45,45,45,45,45,45,45,46,46,46], [19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,20], [15,15,15,16,16,16,16,16,16,16,16,16,16,16,16,16],
      [107,108,108,108,108,108], [46,46,46,46,46,46,46,46,46,46,47], [22,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23], [14,14,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15],
      [120,120,120,120,120,121], [43,43,43,43,43,43,43,43,43,44,44,44,44], [22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,23], [14,14,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15],
      [113,113,113,114,114,114,114], [44,44,44,45,45,45,45,45,45,45,45,45,45,45], [21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,22,22,22,22], [13,13,13,13,13,13,13,13,13,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14],
      [107,107,107,108,108,108,108,108], [41,41,41,42,42,42,42,42,42,42,42,42,42,42,42,42], [24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,25,25,25,25,25], [15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,16,16,16,16,16,16,16,16,16,16],
      [116,116,116,116,117,117,117,117], [42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42], [22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,23,23,23,23,23,23], [16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,17,17,17,17,17],
      [111,111,112,112,112,112,112,112,112], [46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46], [24,24,24,24,24,24,24,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25], [13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13],
      [121,121,121,121,122,122,122,122,122], [47,47,47,47,48,48,48,48,48,48,48,48,48,48,48,48,48,48], [24,24,24,24,24,24,24,24,24,24,24,25,25,25,25,25,25,25,25,25,25,25,25,25,25], [15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,16,16,16,16,16,16,16,16,16,16,16,16,16,16],
      [117,117,117,117,117,117,118,118,118,118], [45,45,45,45,45,45,46,46,46,46,46,46,46,46,46,46,46,46,46,46], [24,24,24,24,24,24,24,24,24,24,24,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25], [16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,17],
      [106,106,106,106,106,106,106,106,107,107,107,107], [47,47,47,47,47,47,47,47,48,48,48,48,48,48,48,48,48,48,48,48,48], [24,24,24,24,24,24,24,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25], [15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,16,16,16,16,16,16,16,16,16,16,16,16,16],
      [114,114,114,114,114,114,114,114,114,114,115,115], [46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,47,47,47,47], [22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,23,23,23,23,23,23], [16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,17,17,17],
      [122,122,122,122,122,122,122,122,123,123,123,123], [45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,46,46,46], [23,23,23,23,23,23,23,23,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24], [15,15,15,15,15,15,15,15,15,15,15,15,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16],
      [117,117,117,118,118,118,118,118,118,118,118,118,118], [45,45,45,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46], [24,24,24,24,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25], [15,15,15,15,15,15,15,15,15,15,15,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16],
      [116,116,116,116,116,116,116,117,117,117,117,117,117,117], [45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,46,46,46,46,46,46,46], [23,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24], [15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16],
      [115,115,115,115,115,116,116,116,116,116,116,116,116,116,116], [47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,48,48,48,48,48,48,48,48,48,48], [24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25], [15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16],
      [115,115,115,115,115,115,115,115,115,115,115,115,115,116,116,116], [46,46,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47], [24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,25], [15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16],
      [115,115,115,115,115,115,115,115,115,115,115,115,115,115,115,115,115], [46,46,46,46,46,46,46,46,46,46,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47], [24,24,24,24,24,24,24,24,24,24,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25], [15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16],
      [115,115,115,115,115,115,115,115,115,115,115,115,115,115,115,115,115,116], [46,46,46,46,46,46,46,46,46,46,46,46,46,46,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47], [24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25], [15,15,15,15,15,15,15,15,15,15,15,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16],
      [115,115,115,115,115,115,115,115,115,115,115,115,115,116,116,116,116,116,116], [46,46,46,46,46,46,46,46,46,46,46,46,46,46,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47], [24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,25,25,25,25,25,25,25], [16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17],
      [121,121,121,121,121,121,121,121,121,121,121,121,122,122,122,122,122,122,122], [47,47,47,47,47,47,47,47,47,47,47,47,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48], [24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,25,25,25,25,25,25,25,25,25,25,25,25,25,25], [15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16],
      [121,121,121,121,121,121,122,122,122,122,122,122,122,122,122,122,122,122,122,122], [47,47,47,47,47,47,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48], [24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,25,25,25,25,25,25,25,25,25,25], [15,15,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16],
      [122,122,122,122,122,122,122,122,122,122,122,122,122,122,122,122,122,123,123,123,123], [46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,47,47,47,47,47,47,47,47,47,47,47,47,47,47], [24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,25,25,25,25,25,25,25,25,25,25], [15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16],
      [122,122,122,122,123,123,123,123,123,123,123,123,123,123,123,123,123,123,123,123,123,123], [46,46,46,46,46,46,46,46,46,46,46,46,46,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47], [24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,25,25,25,25,25,25,25,25,25,25,25,25,25,25], [15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16],
      [117,117,117,117,117,117,117,117,117,117,117,117,117,117,117,117,117,117,117,117,118,118,118,118], [47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,48,48,48,48,48,48,48], [24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25], [15,15,15,15,15,15,15,15,15,15,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16],
      [118,118,118,118,118,118,118,118,118,118,118,118,118,118,118,118,118,118,118,119,119,119,119,119,119], [47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48], [24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25], [15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16]
    ];
    match<[Nat]>(version, level, table)
  };

  public func errorSize(version : Version, level : ErrorCorrection) : Nat {
    let table = [
      07, 10, 13, 17,
      10, 16, 22, 28,
      15, 26, 18, 22,
      20, 18, 26, 16,
      26, 24, 18, 22,
      18, 16, 24, 28,
      20, 18, 18, 26,
      24, 22, 22, 26,
      30, 22, 20, 24,
      18, 26, 24, 28,
      20, 30, 28, 24,
      24, 22, 26, 28,
      26, 22, 24, 22,
      30, 24, 20, 24,
      22, 24, 30, 24,
      24, 28, 24, 30,
      28, 28, 28, 28,
      30, 26, 28, 28,
      28, 26, 26, 26,
      28, 26, 30, 28,
      28, 26, 28, 30,
      28, 28, 30, 24,
      30, 28, 30, 30,
      30, 28, 30, 30,
      26, 28, 30, 30,
      28, 28, 28, 30,
      30, 28, 30, 30,
      30, 28, 30, 30,
      30, 28, 30, 30,
      30, 28, 30, 30,
      30, 28, 30, 30,
      30, 28, 30, 30,
      30, 28, 30, 30,
      30, 28, 30, 30,
      30, 28, 30, 30,
      30, 28, 30, 30,
      30, 28, 30, 30,
      30, 28, 30, 30,
      30, 28, 30, 30,
      30, 28, 30, 30
    ];
    match<Nat>(version, level, table)
  };

  public func errorPoly(version : Version, level : ErrorCorrection) : Poly {
    let size = errorSize(version, level);
    let logs = switch size {
      case 07 [0,87,229,146,149,238,102,21];
      case 10 [0,251,67,46,61,118,70,64,94,32,45];
      case 13 [0,74,152,176,100,86,100,106,104,130,218,206,140,78];
      case 15 [0,8,183,61,91,202,37,51,58,58,237,140,124,5,99,105];
      case 16 [0,120,104,107,109,102,161,76,3,91,191,147,169,182,194,225,120];
      case 17 [0,43,139,206,78,43,239,123,206,214,147,24,99,150,39,243,163,136];
      case 18 [0,215,234,158,94,184,97,118,170,79,187,152,148,252,179,5,98,96,153];
      case 20 [0,17,60,79,50,61,163,26,187,202,180,221,225,83,239,156,164,212,212,188,190];
      case 22 [0,210,171,247,242,93,230,14,109,221,53,200,74,8,172,98,80,219,134,160,105,165,231];
      case 24 [0,229,121,135,48,211,117,251,126,159,180,169,152,192,226,228,218,111,0,117,232,87,96,227,21];
      case 26 [0,173,125,158,2,103,182,118,17,145,201,111,28,165,53,161,21,245,142,13,102,48,227,153,145,218,70];
      case 28 [0,168,223,200,104,224,234,108,180,110,190,195,147,205,27,232,201,21,43,245,87,42,195,212,119,242,37,9,123];
      case 30 [0,41,173,145,152,216,31,179,182,50,48,110,86,239,96,222,125,42,173,226,193,224,130,156,37,251,216,238,40,192,180];
      case _ {
        Prelude.printLn("Error: Invalid error size!");
        Prelude.unreachable()
      }
    };
    Galois.polyNew(Array.map<Nat, Nat>(Galois.alog, logs))
  };

  public func cciLen(version : Version, mode : Mode) : Nat {
    let n = Version.unbox(version);
    let i =
      if (09 >= n and n >= 01) 0 else
      if (26 >= n and n >= 10) 1 else
      if (40 >= n and n >= 27) 2 else {
      Prelude.printLn("Error: Invalid version!");
      Prelude.unreachable()
    };
    switch mode {
      case (#Numeric) [10,12,14][i];
      case (#Alphanumeric) [9,11,13][i];
      case (#EightBit) [8,16,16][i];
      case (#Kanji) [8,10,12][i]
    }
  };

  public func eciBits(level : ErrorCorrection) : List<Bool> {
    let bits = switch (level) {
      case (#L) { [false, true] };
      case (#M) { [false, false] };
      case (#Q) { [true, true] };
      case (#H) { [true, false] }
    };
    List.fromArray<Bool>(bits)
  };

}
