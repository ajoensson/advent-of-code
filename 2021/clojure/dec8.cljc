(require '[clojure.string :as str])

(def segments-to-numbers
  {2 [1]
   3 [7]
   4 [4]
   5 [2 3 5]
   6 [0 6 9]
   7 [8]})

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map #(str/split % #" "))
       (map  #(update
               (split-with (complement #{"|"}) %)
               1 rest))))

(dec8 "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe")

(defn grouped-numbers [arr]
  (group-by
   (comp count
         segments-to-numbers
         count)
   arr))

(defn get-simple-numbers [arr]
  (get (grouped-numbers arr) 1))

(defn dec8 [input]
  (->> input
       (parse-input)
       (map second)
       (map get-simple-numbers)
       (map count)
       (apply +)))

(dec8 input-full)

(def to-num (partial mapv (partial str/index-of "abcdefg")))

(def num-to-segment
  (mapv to-num
        ["abcefg"
         "cf"
         "acdeg"
         "acdfg"
         "bcdf"
         "abdfg"
         "abdefg"
         "acf"
         "abcdefg"
         "abcdfg"]))

; Reference: https://stackoverflow.com/a/26076145
(defn permutations [s]
  (lazy-seq
   (if (seq (rest s))
     (apply concat (for [x s]
                     (map #(cons x %) (permutations (remove #{x} s)))))
     [s])))

(def all-mappings
  (mapv vec (permutations (range 0 7))))

(defn does-map-to [mapping code value]
  (let [segments (get num-to-segment value)
        translated (map (partial get (zipmap (range) mapping))
                        code)
        missing (apply disj (set segments)
                       translated)]
    (empty? missing)))

(defn correct-mapping-for [mapping code-values]
  (every? (partial apply does-map-to mapping)
          code-values))

(defn to-code-values [s]
  (mapv
   #(vector (to-num s) %)
   (segments-to-numbers (count s))))

(does-map-to (to-num "acedgfb") (to-num "gcdfa") 2)

(defn do-stuff [[signals nums]]
  (let [grouped (grouped-numbers signals)
        simple-codes-values (map (comp first to-code-values)
                                 (get grouped 1))
        fivers (permutations (filter #(= 5 (count %))
                                     signals))
        sixers (permutations (filter #(= 6 (count %))
                                     signals))
        correct-mapping (->> all-mappings
                             (filter #(correct-mapping-for % simple-codes-values))
                             (filter (fn [mapping]
                                       (some (fn [signals]
                                               (correct-mapping-for mapping
                                                                    (map (fn [signal num] [(to-num signal) num])
                                                                         signals (segments-to-numbers 5))))
                                             fivers)))
                             (filter (fn [mapping]
                                       (some (fn [signals]
                                               (correct-mapping-for mapping
                                                                    (map (fn [signal num] [(to-num signal) num])
                                                                         signals (segments-to-numbers 6))))
                                             sixers)))
                             (first))]
    (->> nums
         (map to-num)
         (map (partial map (partial get (zipmap (range) correct-mapping))))
         (map sort)
         (map #(first (filter (comp (partial = %)
                                    (partial get num-to-segment))
                              (range))))
         (map * [1000 100 10 1])
         (apply +))))

(defn dec8-extra [input]
  (->> input
       (parse-input)
       (map do-stuff)
       (apply +)))

(dec8-extra "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")

(dec8-extra input-full)

(def input "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(def input-full
  "gabfed acbdfg cd gebcd gdfecb bgfdcae ecdf dcb gdbfe gaceb | dcb dgfceab cbfdeg edcf
cdbeagf df bfcgd bfed cfd fgcbed gcedb cfeadg cegabd fbacg | acbfedg acbfg dfcaeg bfgca
ag agf fbaceg beag acgfe efdcgba afcgbd afced gfecbd bcfge | bcfeg bcdgfae fga egacf
gdfcea edb acefgbd bgcad gabed cdbfea adgef ebgf dabfge be | gdbca be egfacdb adcefbg
dbefg ba edfca fdebga abf ageb fdcagb fbdae cfeagbd dfbcge | ecfda bfa fdgeab fadbcg
cfbage be daecf cedbfag dcafeb eabd ebc dcefb dcgbf gefdca | deafc fbdec gecdfa dfgcb
bdcaeg gde fdeb gadfe ed gfdca febag begadf aedgcbf gcfeba | fbgecad egbcafd fdcag ed
cg gbfad gebc fbdec bdfcg efbgcd efbdca cgfdabe cfg adgfec | befdcag dcbfg bcedf cfg
gefc faedgc adegf fea gcbade fe dgafb fdecab cadeg cfaedgb | dbafg gdebca dbcaeg bdacfe
gdfebc abcefd bcged dgcf efdgb gbfcaed adcgbe bfgae efd df | agfbe cgebd agcedb feagb
fgadec egbf fegdab ef agecdfb baecd fbdae bfadg gdfacb dfe | cebda ebcad dfe efd
acfdge cebgf fbag aegbcf gfc cebgd fg abdcef dcfabeg bcfae | gfba eadbfc efcabg dcgaef
cdfeba bcdage gbcef cdbaf cfadebg dcgbf fgda dg bgfcad dcg | edafgcb gebdca gd cegbdfa
cb cebadf ecbadgf dbgfa cfb cgefd dcfagb dfbcg fdebag acgb | decfg dbgcf fcb adefcb
baedg eafdgb af gfcbd abf bfagd egaf gbecdfa eadcgb dcfbea | gbedfca af gcfbd baf
cg cag gacbe eabgfdc egcbda dgce gdabfe begad agdcfb ceabf | gbfcad bgaced febdag debgca
dabg fdgbc efabc ag afgcbd bcfag fag dbfgaec cfegdb facdge | cfagb fcdbga gaf fag
feabd cgfadbe gdebcf adecb bf fdb abfg dgaefc gefad badefg | bedgacf bfag gdeaf gafb
edfcb becgf eg cbagdfe gfcab egf febcda gfeabd dfebcg cegd | gef ebfdgc gbecf efg
dcgbae fegbac ecg abgcdf bgdca eg gbed cegfdba daecg dfcea | dbge decfa gecad cdfabg
bdg cabdge edagb cgde bgaedcf bdace gbacdf egfab bfdeac dg | acedb gbdeca daebc gdec
gcdab fbacdg fbca edcbag fb cabdgef cfbdg cfedg fgb fabdeg | bafc bacf gfbaedc cgedab
gacedf fcadg agd dgbfea ad gcabf acbfedg fgdec edca cgdefb | adgfc deagcf ecgafd fbcag
defgba cedagb cbgadfe gfcdae faeb fbegd feg geadb dbcgf ef | cbfdg feg ef gef
abgdef fgcbda cafbd gbfc bf bacde bfadegc degafc cgadf fba | bf bcgf ceadfg decba
bfgcad efcag cgdaf dfaegc cgadeb eg egdf gae dfcbgae cabef | fabec feadgcb edcbag ega
bdfea fcab ac eadbcf egdabc bdagfe dcegf dfaec ace gcbafed | egbdaf ca bcaf dafbe
ba gafb adbeg acdfeg dfgae begdc eab dgacfbe eagfbd ebfdac | dgcbe eabcdf eba bcafgde
gfaceb dgfae ca dcfgab dgbcfe fcage bgecf fgacedb cbae fca | bgcfda cafgeb afegc fgace
gfcda egfcda cdabeg gabfe db dfbc dbg bcfadg dfabg bfdagce | abgced db dfbc bd
gdafce bafde cbdfa fea bcgdae bgacfde adbefg bedag efbg ef | ebcgafd gdafbe cegabdf cgfaebd
adfgc bcge acdfbeg bac cb cgfab ebfga adfbeg fcgeba dbcfea | abfgc dgbafe fcdag fcagd
ce gdbfcae bcgadf cegfad facbe bdce bdcfa aec aefgb afecdb | fedbac egdfabc bdaecfg eabdfgc
bfeda fa geafbd bdgef fda cegbdf bgdface gaef cebad cgadfb | adegbcf efag dfeabcg edbgcf
def gcbfd ed ecgbdfa gdefc gbed cadebf cgfae cgdfbe fadcgb | ed fed cbfgda fbceagd
gec agced agef edbca cfdbga dacegf ge baedcgf cafgd cgdefb | fbgcda bcgaedf cgead gce
eabcgd gecbadf bge ge acgbd egad fagcbe fcdbe ebcdg dcgbfa | cebdf geb fedbc geb
gaecfd gaf cfge cbefad fgcdaeb bgefda dbgac dacef cdfga fg | deafcb aedcbgf fg fdcae
acbgef dcbgaf caegfdb be afbcg gafbe bfe ebcfad gbce dfega | bfe decfab fcabde cbegdaf
eafcdg dbgac efgd fcdae eg gadce ecbagf eag efdcba becfagd | gdfe ge acfed aebcfdg
dacegf cbae ac gfbdaec agc gbdfc gbcaed bgead acbgd efdgab | ca ac ac gca
fcd cafe bcade efdcb fc bedfca fgbcdae egbfd cbadfg gadbec | bdgef cf aecf cdf
abgcf bcgafd ge abgdecf edcfa cge cgafe gbae bgcafe ecbfgd | gabe adcbgf gce fcabg
ebfcgd bdeagfc dgbefa fegcd fa ceaf fga dbgca adcfg adecgf | gfceda acgbd af ecgdbf
gabec cgefda efbacd egabfcd bf fba agdfc bfdg bcgfa bdgcaf | afbgc afcdbg acbgf ebcag
begdc gecfd cbgfad cfgda egabcdf fe aefc eadgbf cgefad dfe | afce fe cefa egdbc
efdcag bgcad dbeg dbfgaec ebadc fgabc gd aedbcf dcg cbadge | gd ceabd agbdec gebd
aegb bfcag agcbfe agfcbed bcdfa fagce gcb gb bcfdeg dagcef | bcfedg bgae acefgd cefga
dcfagbe adg ebacgf dfbeag ad fcagd abdc afcgbd cfbag cfedg | dfabgce da cafgbe ad
ead degcb eacgf fbecda adbg febdcag ad bcgdae cgdebf egacd | cefag cdgae fbdgec agcfe
cgbaef bagecfd adbgcf adfeg faedcb fcabe cegfa bgce fcg cg | gacef bgface bdgafc fbaec
acfgd gacbfd bdegfac fcgedb da cadb efgdab agefc fdgcb adg | adg ad aecgf bcgfd
fgdcea fbacdeg cefda dag fedg dg gacbed fbagc gfcad fcedba | gd dcfebag cdafbge gebfdac
bcafeg gaefcd cae gcade ecdf ce dgafc degba gabfdce gbafcd | bafgec gacfdb gdcebaf cdfgab
fcgb afgdec bdecf fg dfbaec cgbfdea dcfgeb dfg ebfgd abdeg | dbega eadcgf febdc cbdgeaf
adcf agdecf dgeac gfcdbe faceg gfceba begdcaf gcd daebg cd | fadc dgcefa ecgdbf acegf
gedfa gbdea bfgade ebcfdg gcedab eabgcdf ef fge bfae cfdag | baef gfe egf degcafb
aedbcf beadg dcgfbea gaecb aedgcf bagefd bed fdgb db degaf | bgfd fegadb bgdea acbfed
fbgca da defbga cebdg bdcga efcbag fgdebca bda fcad bafcdg | ad gcbaf dabgc gbedc
cgeb cfgda eg fcbedga acfebd badegf gea daecbg eabdc caged | ebdac gae bceda dcageb
ge bcdfga cbaedgf gfe gcdfb efgbda febgcd cgfbe gdec afbec | dcge cdge ecdg gbdcf
dcabe ebfgd gda dcfaegb afecdg dageb gbac ag dbcgea acefdb | gda dag gbeacdf ag
gadf fd baedgc gdcba efcab fdb fbdceg gbfcad adbfc daebcfg | dfb fgda bdf cdbgea
bcdfge dbfaec gb bcdef gdeb gcb bfcage fdcgb gafbecd dcgaf | bg bgcdef dbge befgdc
acdb gecbdf cgafb dgbfae dcbafg ab dbgcf adbfegc fab acegf | dcbfg bdca cbgdf dgfcb
acbdfe cabe acdbf badcefg cdbgf ba afb gfcade gedafb fcaed | bfa gbdaecf ba adecf
ae bdafc bceaf gdbafec bfgec eab dabfge fgbdca daecbf eacd | ae becgf cead bfcea
bcged dec bfceg acbd faecgdb daebg dacbge dfaebg gfadce cd | ecd dc gfcbe adbfeg
ecadfg bfdge fe fde bagcfd fcbe bgfdc geabd bfcegd dgbcaef | fcadgb bgade fe fegadc
degabf face fcd cf bfdegc adcefg cdgfa cdagebf afdeg bcgda | fcdebga fc caegdfb feadg
dfage dfec df gabced eacfdg fdg fgbecad cadeg gfcabd aefgb | cdef fegdac fd agfeb
da bfgac adf gefdc cabfgd bgfeac afebgd cgfaebd cgdaf adbc | fda dfgce gfabce faedbg
dgfba gadefc acegdfb bgd bg cgba facdg bgdfca febdgc eadbf | gdb dagecf fbdgac dafgb
ab gecaf ebfadcg bagfc cbgfd dcfgea cab efabcd eagbcf abeg | acb fbdeac abc agfebc
gc eagfb gec aegcbd acfed gdbceaf cgfea dcgf cgefad cdbfea | dfgc gefba cg aedcfb
deafc gbcf bfdeg dcafgeb efdcb acbged fgedcb cbe bc gefbda | cdfea fbgc fedcb ebfcdg
cd dfaeg dgceba gdcebfa gcfde gbdfce dcg fbgce ceabfg bdcf | cd dc fcgbe deafg
fd cfeab bdecf abdgec fdbecg dbfg def faegdc fagcdbe bcged | edcgb fd fegbdac dcebf
gcfbd fa fbadc abgedc cadfbeg fda afbe bcead dcafge ebfdca | fdgace gefadbc acbfd befa
bgcedaf afbegc eg dfbeac ceg fgcdae efcda ecgdf gcfbd daeg | decfa deag gcdfe ge
cgabdf af abfe agedbc ecfdg bgdea fagbecd fadbge dfa dgeaf | fa dbagce af af
egdabfc dgbac gefdca bcef gcfbed cf dbgef dcfgb fdc afbedg | becf gdeabfc dbfgcea efbgd
feb abegfd agfedc cbfge fagec bf gcbde afcb aefgbc gfbecda | bcfa bf fb egfacb
fage cgbfd gcfde gedfca fce fe adcge dafbceg aecdbg cdebfa | fgae gaef cfeabd cfdbg
gdcfe ed eagdfb dcfgbe bagcefd ebcd fdbgc deg gecaf gbdafc | gebdcf efgdba gcfebd de
debfga dcbe bcaeg aecbgf acedg ed dbgcae dgecfba cagdf ade | geacd ecbga de becadg
abegc fe efbdca aecfg adfgce efa bgafcd gdef gfadcbe fdcga | ef dcgbeaf fgcad faceg
adgfeb fbcge fbacgd acb abdcef ca ebcaf faedb caed gcbeadf | afecb acbdfe efadgb cbgfead
cefa daefbc fdecb aefdb gfbceda bgdafc debfga bgedc fc cbf | bfc ecfdba adefb ecaf
debc dfbage caedg cafedgb fbacg gbd agbdc gdafec db deagbc | bd bdg befdag dcfage
gacefb edbac gdef gfbeacd gfecb gacbdf fd dcfeb bgefcd cfd | cgbedaf gdfe gecfb cfd
geacd eb ebg gfbcade egfcdb daeb dfacge eabcg fabcg cdabeg | gdcaef ebg beg daeb
gadbfce gfda abcgfe gd bacgf abdce dgb dacfbg dbagc fbegdc | ecbafg bfgac gcbfa cbgfde
aedgcb cdaef efg egcba fcage fgbeac daefgb bfgc gf gcbeadf | fecga edagbc gf bdefagc
db cgebd efbgdc egfcda ebdf gecfd fdebcag dbc abfdcg gabce | db cedgbf aecfgd debf
ecfadg efbagc feg cgde fbcdag ge gadef bcgadfe cafgd aefbd | fbgcae afcdebg edfab eg
gebfad bgeca fdce bdgceaf cd debgc bcd dbgfe bdfcag bfgdec | abgec cbage facdgb dc
dbfgcea eacg edacf dfgec bgdcfa daefb fedbcg cad ca eacdfg | cad ebgdcaf cage eafdc
decafg abdcg bgdcfae fa daf bedfcg cfdba cfdaeb abef fbecd | dbafgce afd fa caedfg
dfcbgae fgecb abegdc beafdg ecd efdgc cfeagd cdfa eadgf cd | egdaf cdfa decgaf cdaf
gbafed cegfadb cage efcabg cgabf gfebc dacfb bdefcg ag gaf | fgcab fbdgce bgacf agbfcde
badgc bea be adcfbe dgbecaf ecgb cdgbfa dbega gdfae daegcb | cebg adgcbf bcdga agbcd
gbaced fadcb ag dfebgca gac bdgfec acgbd gdceb aegd gcbefa | adge gaed gca ecbdg
bgdce gbdcfe ge badec gacfbd egb fdcbg fgaecbd fcbeag dfge | gfed cefgbda ebdgc gcbdf
aedfc cgde cgfaed bgfad ge bcfaed ecgabf gae egbfacd geadf | fdeag febcda cedg cafdbe
agf fegad fdgcae fcead fg abcfed cfeg agdbe fcbdga degbfac | adebg ecfda afg fg
bdfegc aefg bacged fegdbac fgcad cbfad ag agd fegdca fcedg | feabcdg bdcefg gbfacde edgfac
aec cefbag dabfgce dgcae degacb ae bgced dbea fedcgb fdacg | ecgbda ebcgd daceg cedgb
dcbfgea decba fadegb ebfag cef fagbec fc bcgf ecdgfa febca | bgeadf fgebda gcdafe abcde
bfcgd degfbac afebc def cfdega ebgd ed bfced cedbgf fdcgba | dgbcef fdcbe gafcdb gbfdec
fge abfcdeg dfega fg eabgd fdgc fgaedc ecadf becgaf befcda | edgaf fcdg dbfcea bcefdga
bgcafe bdfecg acedf dcfbe agcdfb be bfcegad fbe ebgd gcdbf | fbe feb bedcgfa fdbce
be cabdfe adgce fbcda cfeb aeb acfbegd abgfed ebdca adgbcf | egadc eb fcadeb bcedaf
gfbea ecbf gcefa ec cdfag fdgeba aec eagbcd bcfgae afdgcbe | gbadcef afcbegd acgef dfgbea
gb dgbf cegfda bfcgae dceba afcgd cagbd afdebgc gcbdfa cbg | bg fbdg cbg bacgfe
efgdcb cgefabd fc abedc bgefd deagfc cbfed cbfg ebafdg fce | febdc fgedcab bcdae fdcbe
cafbdg gdebc beadf dca acebd ac fgdeba ecfabd gdefcab ceaf | efdcgab gabefd dcgbe gdecb
fbd adgbe fegcb afdcbg dcfgbea egfdb fdce fd fdbgec caefbg | edfc afbdcg abfegc fdbeg
cbgdaef eabdg afdb cbdeag ecfgb fd edfgba fcdage efbdg def | dgfcae dgebf bcadge fd
eb dfcge deb cbgdfa abef baefdg abdgf fbdeg fbcgade agbcde | afeb dfgba agebcd dbeagc
gcbde eadfbc gbeda edcbfg ae aedbcg afcgbed afgbd dae agce | cbegd bafdg bdega gbadf
faegc adbcfg bgfaec edgcb geadc dafgebc ad dfae ecfdga cad | acd da fcbgea gdceb
ga daecfb afgbdc dcaef gcfbe gedafbc edafgc cgafe agf gaed | ga fcbadge fdebgac egacf
cdgfab gaedb egfbc ebdfag dgc dc ecad eacgdb dfegcab bcdge | efgbc dc gcd gcebad
edcfg befacd cdgae bcgfed dae gfad agebc bdcfeag da fcgeda | fgad edbcaf fcadeg ceagb
ba afegcd edcfb cba baefcg fgba acdgbe bgafedc ebacf ecfag | gcadfe cfdbe cgaebd beafcg
cegfa edcb be cafeb fagdbec fbecda cdfab dagbcf dgeabf feb | aebdfgc eb bdec cbaef
efdba bacde dce dc beagc dcfb dagbef adgfce afedbc dgafebc | dce deacbf dbaec dcbf
fcdebg ce caef dabef eacbd gaebcfd ecb dbfcae cdgab defgab | dabgc ceb cafe bfdea
ebdcagf gefcb bafe aecgdb be fgeac geadcf cbe dcgfb gbacfe | bce be gdfcb eb
bfgead deacb fae fbcdag fe defgbac baefd bgfda gdef bacgef | aef fdgab fgcabed dabgcfe
bg fgadec debg fbg cadfb dgafebc eagfd dabfg fbegad gfecba | bacfge egcabf gafdb dacgfe
bgdce ecba abcdfg gbe bafged cfdeg eb becfdag bgadce cabdg | egb bace fedgc facdegb
gbade agf efac fgdaecb gcfabd bfecg bcfgea gdbfce fa gfaeb | gdfeabc febagc fa afce
bfcegd fbdeag ga dcabeg egfa dagfb efdgb dbcagef dag fcbad | bcfgaed edcbgf dga dacgeb
bdcfga db gadcfe bgdcf gfceb cadbge adfb cbgdfae bgd dfagc | gefbc dgfaec cgfeb db
degabf gbea ecfbd gdfbac aed fcegda gdbaf ebfda ae edfabcg | bcfed ae dgfecba dgcfae
begaf agdbfc fagcb be edgfbca efcb afgde bae gfbcae agcedb | be efbc afbgdc begdac
afebg ecfab becfd gbaefc cgfa ac edacgb bedgfa febcgda aec | fecbd cbfeag efabg gbdeaf
cfdage ef dceagb feda fce ecadgfb ecgad gcfde gfbdc ecfbga | dbfgc efad fead bgdcaef
agdbef fb bcedfag cgdba bdegcf fgead fbd gfadce befa bfadg | dgabc fb dfb bf
cfebgd cabdf dgcaeb egcbaf ae cea eadbc fedbagc aedg cedgb | gecdb ace aec ae
cbe bgaced cgfba baef fgabcd fgcde fgabce eb efbcgda bgfec | abgfdec bgafc badefcg ebc
ecagb dfce cf fbc dcagbf befca acedfb gdeacfb eadbf dgafeb | fedgba dfcabe cegab ecagb
fa agbdc cgdaf aedf efgcd efdacgb gbcfde afg acdgfe gfabce | fa gaf fead fgecd
egacb cgbdaef ce bec gbafc dgcfab egbda dfebac fceg ebacfg | gbcea acbdef caebgf efdgcab
dafeg cd febadg acefb agcd bfcgdea dcfgae ecdfa ecd dbfegc | becaf dgac becaf egcdfa
badc bc cfaed ceb dbgfeac gadefc fgeab fgbedc acbfde abcfe | afegb efbdca cdfbea bdegcf
bafedg dgbceaf gcdef dfeabc abfdgc gcab cfb cdfgb dgabf cb | bc cb fgdba bcf
dfabg dfgbca cd bagdefc afbce gdacef bdcg dca bafcd efadgb | dca bcadf bdgfca adfecg
acgbd acfgde fbgedc fbgcdae fdcabe fdcge fcb fb fbge cdbfg | cbf gfdec fbc acfdeg
eafbdg eadbgc bde gcdbaef gdceaf ecdag fgecb cgbed bd adbc | aecgdfb bd cgaed dbe
dg fcaebg gbeaf gcdfea fgd fgdab cfabd ebdg fcdgbae gbedaf | fagbce dcegaf afecbg cegbfa
gbd efcagd gb bgeda fbade bfecgd eabdgc daceg gfecbda acbg | egbfcd fegcbd egdba aedgc
bag efbgad ecgdab edabc adbcg agec ag feabdc dgfcb efbgacd | febdcag cgdfb agb gdcab
fegdabc becafd dcefa eagdc bcfae fd fde dafb febcag befcgd | cbdfea fbda edf fed
gdcbf cafbeg bgdface ad eadf eafbdg fdgab gdbcea egbfa dba | gbfeac ebagf bdacge gbecad
ebfcag ecfgadb efbagd egb dbfagc cbfe be agebc bfacg agdce | beg becf gcbaf bafgc
bgedca eacgf dec afebcd dc bedag adgbef ebcgdfa dgbc cdgea | agbdfe ebagd dce bgdc
adecgb bdeaf gdefa gdbfeca fgbe aedcbf egd eg cfdga afdbge | dcfeabg cfgad gefad gfbaed
cafe ce abecdg edc bdcfg afcdgeb beagfd ecdfb bfcdea fdeba | ce face ce ecd
gf efadc gafcde cfagd cgf deafbc dgef bcefag gbacd dagbefc | efacd fgc cedaf agdcf
dgfebc bad ba eadgb agecdb gadfe abcg cgfaedb gbecd ebcdfa | ab cfegbd ebdgac bcfegd
baegf gdafe gedb edafgb gfaecd ceabfd eb abe gedbacf fagcb | fabgc fgdae egfad eb
dbfeac bcegaf cabfdg fbcga abgedcf dbgc aegfd db fbd afbgd | dcbg afgdcb dcbafe dabgf
df dfa fbdae becda agbfe gfabde gcefad gdfb fgcbea abfegdc | daf dgfb fdgb aebcd
adebf ec cfeda aec gefdca gdcaf dgfaecb fgdbac cegf bdcega | ecgf cae faedc edbgcaf
de efcdgab gcdabf deaf gbfad bdecgf dbe bgaec gabed ebdgfa | cfdbgae bcgfda de bfadg
gdefca fedacbg fc abecgd bafgd febdca cbaed acdbf ebfc dfc | fbcad dfbacge baedgfc feagdbc
gfdce fecdgb ac daceg fdac beafcg ebcfdga gbdae agc fgceda | cga efdgc gaecdf bcgfea
agebdfc ebgfad deacb eab edgfca cbfa afced ba cfdeab ebdgc | ab bdcge aecfd aeb
gbaf gadecbf gbcad bdcfa gcfbad bf cfade bfd dagcbe cbgdfe | dbafc bf fbacd bfacd
dbegc dfecbg cfd geadcb cgfe gbdaf fc abefcd aefgdcb fdgcb | cdgbf cf fadbg dfc
gdfea cgbfd bfca cedbga bfadcge fecdbg cgfabd ab bgafd dba | fecdagb agdcbf bgcedf cfebgd
gbacfd de ecgbfad gedabf ecadbg dea dagcb badec edgc efabc | ed ed gdce gdfeba
fgdeb bgc gcdaf daegbf aegfcbd baefcg gcdfbe bc fgbdc cedb | gacfd gcadf gbfdc gcdaf
daefbcg de cgadfb aebfg cfeadg edac efagd afdgc dfe ecfbdg | geafb cdgaf ed fde
ega cbgda dfbea afgbed bedafc febdgca ge cfgbea dgabe fgde | dfge gae egdba agcdb
gfdcb fdbe be aefdgcb ceb abdgec fbgdce ecgaf gcdbfa gecbf | cgeaf cgfae bec gdcaeb
dafeg bead fad cfgea da dgbfe acfbdg fdebga bdgecf fbdgaec | gaefc cdebafg ad gcafe
fbdca ec agebf ecf edac febgcd bgfadce fabcgd debfac cebfa | cadbfg fec fedcagb bacfgd
afbdge bgfadc adgfce fdega cd cbaef decfa dbacgef edcg dac | edabgf gedc cfdega dca
bagecf fecdab aecbg cf efgbdac ebdgac fegda fbgc cfe gceaf | fc cf gcbea cef
fcadbe aedbfg afdbc dfegcb acfe fbgedca bcdag fc edfab bcf | edfcagb cfdab fedcgb acfbd
daecb dgab dbgceaf feacb cgdeab da acd adecfg bedgc efdbgc | fgeabdc eacbd da ad
efd adefcb ed fagced dbagfc bafgcde fedag cegd faebg gcdfa | eadgcbf gfcda fbadec fagdc
edbgc cbagfed cgbdfa ae baged dgfba bfea fegadc ega fgaedb | dgafebc dcgbaf gdebc cebdg
gcd cg fecg bgead bdecg fgcbad fgbdce decbgaf bceadf dbfce | bgeda dgbea bfcdag fecg
bcaged fgcbed ecaf bae cgfebda fbacde ea eafdb bdafg dfecb | acbdgef dcefb dbfaec fadgb
cdb defc abgfc faegbd dbacfge cdbfea gbceda dc bafcd afbde | fadecbg cd cd fbadc
dgfce eafdcgb fcgae gabdcf gde ed cbgfd baecdg efbd cdgebf | ed baegfcd dfgcb dgbcf
fgedba fageb cgeabf cabgf fgc fcae aebfdgc fc badgc decgbf | cfg abgfc cfbga fc
abegd cdg gfbadce debgc cg cdfgeb ecfdba cdbgaf bcedf egcf | gfec egcdb fceg ecfg
gefcb eacfg dcebgf ag fceda cgdfab cga gbea gbecfda fegcab | ga egba gdafbec cafeg
efgdacb gfaed gfeba faegcd deabgc debafg ba fbceg dbfa agb | egfcb begfacd gab cfbaegd
abgcfde dgebf decfb efg fabedc eg gebc cgfebd faegdc fdgab | bdefc cegb fbedg eg
ebacdg dcbge cgab gc dfbgae adcfeg edbcf gcd beadg bfcdgea | acbgde gc gc ecbgd
af acbegd fadc bfdge gedfa gebcaf aef baecfgd cegda cegdfa | dgeac cagbef cdgfeba dfegb
cgdbef dcebf cdgeaf egcfabd bfge ebfcad fdgbc fg dfg adgcb | ecfdb fgeb fg gf
decfb bc gfedc fcagbd bgec ecgdabf degfcb ebafd afcdeg bcd | aefbd fdceg dcb efgcbd")
