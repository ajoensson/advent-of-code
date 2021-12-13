(require '[clojure.string :as str])

(def demo-board [[0 0 0 1 0 0 1 0 0 1 0]
                 [0 0 0 0 1 0 0 0 0 0 0]
                 [0 0 0 0 0 0 0 0 0 0 0]
                 [1 0 0 0 0 0 0 0 0 0 0]
                 [0 0 0 1 0 0 0 0 1 0 1]
                 [0 0 0 0 0 0 0 0 0 0 0]
                 [0 0 0 0 0 0 0 0 0 0 0]
                 [0 0 0 0 0 0 0 0 0 0 0]
                 [0 0 0 0 0 0 0 0 0 0 0]
                 [0 0 0 0 0 0 0 0 0 0 0]
                 [0 1 0 0 0 0 1 0 1 1 0]
                 [0 0 0 0 1 0 0 0 0 0 0]
                 [0 0 0 0 0 0 1 0 0 0 1]
                 [1 0 0 0 0 0 0 0 0 0 0]
                 [1 0 1 0 0 0 0 0 0 0 0]])

(defn folded-halves [board pos]
  [(take pos board)
   (reverse (drop pos board))])

(defn fold [board axis pos]
  (->> (case axis
         "y" (folded-halves board pos)
         "x" (apply map vector (map #(folded-halves % pos) board)))
       (apply map (partial map +))))

(defn parse-input [input]
  (let [[coords-str _ folds-str] (partition-by #{""}
                                               (str/split-lines input))
        coords (map #(->> (str/split % #",")
                          (map read-string))
                    coords-str)
        folds (map #(-> (str/split % #" ")
                        (nth 2)
                        (str/split #"=")
                        (vec)
                        (update 1 read-string)) folds-str)
        [max-x max-y] (map #(-> (apply map list coords)
                                (nth %)
                                (->> (apply max)))
                           [0 1])
        base-board (vec (repeat (inc max-y)
                                (vec (repeat (inc max-x) 0))))
        board (reduce (fn [b pos] (update-in b (reverse pos) inc))
                      base-board
                      coords)]
    [board folds]))

(defn dec13 [input]
  (let [[board folds] (parse-input input)
        [axis pos] (first folds)]
    (->> (fold board axis pos)
         (flatten)
         (filter pos?)
         (count))))

(defn dec13-extra [input]
  (let [[board folds] (parse-input input)]
    (->> (reduce (fn [b [axis pos]]
                   (fold b axis pos))
                 board folds)
         (map (comp (partial apply str)
                    (partial map #(if (zero? %) " "
                                      "#"))))
         (reduce #(str %1 "\n" %2))
         (println))))

(dec13-extra input-full)

(def input "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5")

(def input-full "428,532
925,404
882,362
44,836
191,714
412,36
688,523
522,842
1054,359
523,413
388,638
60,495
758,148
381,42
1017,80
279,696
848,508
922,638
381,852
887,822
500,672
969,526
986,760
102,187
1232,303
758,596
412,820
320,296
935,715
310,308
129,422
383,485
919,786
261,318
745,863
279,179
628,266
1185,798
388,190
790,788
775,254
771,491
1034,513
147,689
1215,178
1263,739
949,228
152,852
536,444
359,705
946,780
21,863
395,864
527,603
167,856
877,516
1046,586
1146,287
651,604
32,383
75,800
364,780
306,470
689,332
310,362
759,451
70,198
748,639
937,795
515,453
1113,24
873,819
706,99
1031,179
60,88
211,786
1051,626
518,490
341,722
413,598
301,772
238,38
159,318
1265,547
572,803
557,241
947,250
490,781
1054,191
1158,148
388,358
575,191
961,826
723,268
314,402
208,612
433,348
549,515
361,814
410,4
618,324
109,75
1190,599
207,864
78,79
1151,455
701,362
135,266
1250,399
1072,808
616,298
1163,653
1200,19
951,789
518,404
698,890
1029,249
820,781
930,555
92,479
739,367
623,677
252,108
372,612
668,884
842,880
36,794
395,30
1305,742
987,380
1164,66
922,582
1019,512
678,273
187,389
1136,479
243,527
209,476
446,810
1245,656
1068,372
341,526
770,635
92,150
852,175
619,473
195,77
229,464
218,705
887,110
206,519
684,333
221,378
31,217
477,498
967,448
907,551
947,306
1072,114
1153,71
987,828
472,504
937,520
952,656
795,291
795,697
1120,14
1019,382
279,248
1181,444
699,833
535,254
629,424
1176,508
515,155
1022,359
253,123
701,84
498,72
1228,834
65,689
470,847
216,875
836,756
1279,229
1221,703
406,806
699,285
415,373
154,448
612,331
632,173
67,142
492,172
512,120
1185,350
537,28
1079,9
95,379
231,254
398,807
219,816
189,333
386,301
858,456
361,142
242,336
1103,864
1165,282
957,794
256,817
743,584
412,372
261,211
65,459
13,789
1190,410
1124,61
681,526
31,677
585,304
679,155
13,626
594,120
415,521
951,105
179,848
895,231
960,497
1237,568
77,374
678,173
1222,562
565,863
33,218
1116,432
559,110
658,87
947,25
43,61
937,732
464,880
969,78
622,819
406,634
796,61
736,387
681,470
641,341
1144,820
544,875
626,701
1215,640
1218,479
192,834
1031,226
437,819
467,837
570,134
1260,795
760,276
259,864
877,277
135,42
36,548
31,789
147,205
840,847
269,861
509,612
259,101
1028,164
423,784
1020,873
490,315
492,378
412,558
375,491
564,399
324,606
740,894
594,806
812,72
102,707
989,857
460,550
412,858
457,24
663,11
184,159
457,712
616,477
348,47
239,455
279,198
385,705
587,414
1118,386
929,747
1235,416
900,4
786,623
36,570
281,700
1034,717
303,453
50,795
776,372
572,333
863,618
622,607
395,133
1260,39
937,645
139,645
798,679
281,697
428,362
467,603
604,263
734,634
551,451
1102,58
164,579
1006,497
616,701
53,241
872,427
1302,726
576,602
324,288
216,467
45,547
1049,766
243,303
53,721
78,558
13,618
609,621
1062,806
305,68
1136,191
887,885
157,71
70,696
244,442
949,142
47,739
880,596
45,697
2,830
744,758
1196,187
296,572
179,400
256,369
912,646
1290,47
304,397
160,306
290,138
443,633
947,158
43,462
1094,331
845,122
867,633
31,705
324,746
508,138
607,646
937,84
735,255
1200,875
1192,276
349,68
408,91
229,502
229,397
525,24
793,561
932,177
647,11
706,263
50,487
1225,547
673,264
1089,378
773,756
986,148
242,558
1290,847
97,379
731,106
238,114
875,78
850,717
67,480
975,683
95,672
1089,617
1171,645
53,465
1275,645
895,663
10,255
160,484
703,646
1240,807
1012,197
523,553
1290,582
735,252
865,583
872,467
646,56
668,458
13,520
1009,772
1034,316
965,182
726,817
1308,87
540,259
735,642
997,78
92,253
734,196
221,635
576,303
77,520
1031,198
1059,551
545,523
447,618
401,120
378,401
1081,877
281,249
169,185
820,315
681,368
977,366
10,395
358,238
415,231
224,596
517,561
1267,432
398,646
438,467
293,114
1215,379
1257,465
612,779
525,444
25,526
345,264
1213,379
534,596
1208,806
716,774
944,87
887,784
957,346
480,36
1121,561
184,774
1279,189
790,386
443,338
1218,639
32,735
1019,420
562,422
334,30
33,80
965,264
1051,101
1279,665
1144,606
535,192
146,586
1265,473
33,814
67,515
801,282
1228,333
536,724
252,786
862,331
694,535
338,499
129,472
1068,336
88,758
962,424
1170,733
1250,358
282,164
698,779
1230,248
1150,858
187,827
537,583
845,781
810,0
435,78
20,586
333,528
313,301
256,191
455,43
415,567
1059,103
462,60
189,109
1150,306
559,603
549,281
856,794
1039,100
863,52
944,417
536,170
499,479
1000,308
283,14
515,697
552,124
1205,525
333,857
753,653
152,596
576,292
1278,287
485,295
269,705
1094,875
241,99
298,197
977,857
622,523
351,827
38,477
848,732
306,495
587,142
1111,851
514,733
1205,515
587,773
271,794
1068,36
706,631
164,287
189,785
925,267
70,421
1104,563
609,532
539,627
208,170
775,702
415,663
835,46
458,175
310,586
1113,450
1183,800
102,134
216,147
105,187
1266,388
1279,637
715,161
1205,826
522,550
783,458
354,350
576,784
734,740
1129,406
1226,760
935,198
1140,520
979,739
845,113
641,520
740,50
612,115
1232,558
987,66
105,707
378,347
324,8
944,872
425,464
682,875
1215,222
353,445
853,712
1205,379
1059,247
748,479
420,191
835,848
525,870
736,646
261,576
251,247
751,110
689,80
139,291
723,773
758,124
281,197
927,409
840,868
537,866
751,603
271,485
947,698
1151,318
688,819
907,654
109,819
197,892
35,645
734,154
790,60
401,351
313,520
1138,222
845,794
433,98
656,42
1099,786
1007,453
851,77
783,352
328,107
473,406
885,654
323,66
167,38
669,480
567,584
1308,830
1228,113
733,72
1019,474
2,298
363,25
575,703
363,158
137,100
259,416
242,50
738,289
1066,803
1020,780
331,439
957,445
743,663
380,339
371,61
222,618
468,432
535,842
1081,502
226,362
224,298
291,535
1195,871
725,304
734,751
23,739
535,397
447,842
448,331
291,158
1004,424
977,264
438,595
89,15
371,304
616,193
120,599
1240,817
281,362
383,346
388,582
406,536
1277,80
145,52
32,287
1235,800
912,87
32,299
893,792
765,75
962,343
1019,333
192,508
330,311
986,288
559,336
248,634
157,80
1071,455
276,401
594,658
281,795
470,420
843,603
313,78
711,393
1139,820
124,759
805,610
85,347
833,396
1190,779
93,366
1173,100
495,128
162,154
1073,123
562,639
291,561
1151,576
293,786
585,590
1307,738
1123,827
661,266
514,161
1066,386
570,894
725,142
792,404
259,478
36,52
251,240
43,432
190,462
917,576
244,4
259,268
1141,332
855,291
1004,47
162,68
502,820
95,178
1138,670
266,326
666,843
1084,84
1051,581
1220,847
1027,733
1153,103
437,75
708,299
1274,346
264,586
669,520
890,191
247,252
798,215
300,19
166,606
216,651
385,267
248,751
986,606
950,147
468,880
243,815
1201,819
423,110
939,164
761,505
1215,515
179,429
1233,374
551,443
137,337
1020,394
1063,642
1208,760
972,499
922,806
497,103
420,705
92,639
1081,430
771,788
654,225
751,178
375,179
1265,697
497,743
331,455
20,84
70,817
1192,450
835,465
415,788
1285,449
986,135
699,609
1222,786
629,526
291,512
1133,172
924,593
5,742
840,287
893,123
242,396
846,462
410,605
88,562
1215,626
1036,523
571,367
1079,885
425,465
1158,852
1218,641
692,394
1260,99
515,291
990,416
276,130
1161,284
935,226
843,505
738,803
1310,852
3,738
288,359
783,291
474,756
689,590
125,350
269,259
102,806
82,60
244,803
977,630
927,548
1274,570
1196,707
698,563
115,871
279,403
87,231
1068,74
652,746
1057,95
1282,417
485,709
1304,598
1282,193
95,515
647,435
433,277
895,106
70,807
244,91
646,838
748,33
587,827
641,480
1278,299
1171,301
1297,268
50,347
1089,169
701,25
211,338
1243,379
13,268
348,424
595,733
616,596
787,525
120,779
423,864
634,532
242,844
873,75
1267,730
502,746
281,173
822,750
402,108
694,701
206,666
927,841
341,816
594,47
1067,815
118,892
166,807
95,626
403,654
454,100
59,609
895,788
721,241
676,532
74,310
333,485
1029,84
830,36
599,501
1218,749
417,123
1057,123
694,193
324,512
179,494
842,432
979,439
155,479
1161,610
187,515
930,339
1183,291
393,318
1290,532
788,550
785,291
947,830
1186,759
867,556
748,861
460,177
713,852
358,572
507,633
1265,421
631,155
1243,414
1222,108
229,430
715,285
818,131

fold along x=655
fold along y=447
fold along x=327
fold along y=223
fold along x=163
fold along y=111
fold along x=81
fold along y=55
fold along x=40
fold along y=27
fold along y=13
fold along y=6")