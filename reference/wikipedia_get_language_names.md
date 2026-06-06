# Get language names

This function sends a GET request to the Wikipedia API and returns the
language names as a dataframe.

## Usage

``` r
wikipedia_get_language_names()
```

## Value

A dataframe of language names.

## Examples

``` r
# \donttest{
wikipedia_get_language_names()
#>          language_tag                                       name
#> 1                  aa                                       Afar
#> 2                 aae                                   Arbëresh
#> 3                  ab                                  Abkhazian
#> 4                 abe                            Western Abenaki
#> 5                 abq                                      Abaza
#> 6            abq-latn                                      Abaza
#> 7                 abr                                      Abron
#> 8                 abs                             Ambonese Malay
#> 9                 ace                                   Acehnese
#> 10                acf                        Saint Lucian Creole
#> 11                ach                                      Acoli
#> 12                acm                               Iraqi Arabic
#> 13                ada                                    Adangme
#> 14                adg                              Andegerebinha
#> 15                ady                                     Adyghe
#> 16           ady-cyrl                   Adyghe (Cyrillic script)
#> 17           ady-latn                      Adyghe (Latin script)
#> 18                 ae                                    Avestan
#> 19                aeb                            Tunisian Arabic
#> 20           aeb-arab            Tunisian Arabic (Arabic script)
#> 21           aeb-latn             Tunisian Arabic (Latin script)
#> 22                aec                              Saʽidi Arabic
#> 23                aee                           Northeast Pashai
#> 24                aer                           Eastern Arrernte
#> 25                 af                                  Afrikaans
#> 26                afa                      Afroasiatic languages
#> 27                afh                                   Afrihili
#> 28                agq                                      Aghem
#> 29                aha                                     Ahanta
#> 30                ahr                                    Ahirani
#> 31                aig       Antiguan and Barbudan Creole English
#> 32                aii                       Assyrian Neo-Aramaic
#> 33                ain                                       Ainu
#> 34                ajg                                     Ajagbe
#> 35                ajp                     South Levantine Arabic
#> 36           ajp-arab     South Levantine Arabic (Arabic script)
#> 37           ajp-latn      South Levantine Arabic (Latin script)
#> 38                 ak                                       Akan
#> 39                akb                              Batak Angkola
#> 40                akk                                   Akkadian
#> 41           akk-latn                    Akkadian (Latin script)
#> 42           akk-xsux                Akkadian (Cuneiform script)
#> 43                akz                                    Alabama
#> 44                alc                                   Kawésqar
#> 45                ale                                      Aleut
#> 46           ale-cyrl                    Aleut (Cyrillic script)
#> 47                alg                       Algonquian languages
#> 48                aln                              Gheg Albanian
#> 49                alq                                  Algonquin
#> 50                als                                  Alemannic
#> 51                alt                             Southern Altai
#> 52                aly                                   Alyawarr
#> 53                 am                                    Amharic
#> 54                ami                                       Amis
#> 55                amx                                 Anmatyerre
#> 56                 an                                  Aragonese
#> 57                ane                                    Xârâcùù
#> 58                ang                                Old English
#> 59                ann                                      Obolo
#> 60                anp                                     Angika
#> 61                apa                        Southern Athabaskan
#> 62                apc                           Levantine Arabic
#> 63           apc-arab           Levantine Arabic (Arabic script)
#> 64           apc-latn            Levantine Arabic (Latin script)
#> 65                apw                             Western Apache
#> 66                 ar                                     Arabic
#> 67             ar-001                     Modern Standard Arabic
#> 68                arc                                    Aramaic
#> 69                are                           Western Arrarnta
#> 70                arn                                    Mapuche
#> 71                aro                                     Araona
#> 72                arp                                    Arapaho
#> 73                arq                            Algerian Arabic
#> 74                ars                               Najdi Arabic
#> 75                art                      constructed languages
#> 76                arw                                     Arawak
#> 77                ary                            Moroccan Arabic
#> 78           ary-arab            Moroccan Arabic (Arabic script)
#> 79           ary-latn             Moroccan Arabic (Latin script)
#> 80                arz                            Egyptian Arabic
#> 81                 as                                   Assamese
#> 82                asa                                        Asu
#> 83                ase                     American Sign Language
#> 84                ast                                   Asturian
#> 85                ath                       Athabaskan languages
#> 86                atj                                  Atikamekw
#> 87                atv                             Northern Altai
#> 88                aus            Australian Aboriginal languages
#> 89                 av                                     Avaric
#> 90                avk                                     Kotava
#> 91                awa                                     Awadhi
#> 92                axe                                Ayerrerenge
#> 93                axl                      Lower Southern Aranda
#> 94                 ay                                     Aymara
#> 95                ayh                            Hadhrami Arabic
#> 96                 az                                Azerbaijani
#> 97            az-arab                Azerbaijani (Arabic script)
#> 98            az-cyrl              Azerbaijani (Cyrillic script)
#> 99            az-latn                 Azerbaijani (Latin script)
#> 100               azb                          South Azerbaijani
#> 101               azj                          North Azerbaijani
#> 102                ba                                    Bashkir
#> 103               bad                            Banda languages
#> 104               bag                                       Tuki
#> 105               bai                         Bamileke languages
#> 106               bal                                    Baluchi
#> 107          bal-latn                     Baluchi (Latin script)
#> 108               ban                                   Balinese
#> 109          ban-bali                 Balinese (Balinese script)
#> 110               bar                                   Bavarian
#> 111               bas                                      Basaa
#> 112               bat                           Baltic languages
#> 113           bat-smg                                 Samogitian
#> 114               bax                                      Bamun
#> 115               bbc                                 Batak Toba
#> 116          bbc-batk                  Batak Toba (Batak script)
#> 117          bbc-latn                  Batak Toba (Latin script)
#> 118               bbj                                    Ghomala
#> 119               bcc                           Southern Balochi
#> 120               bci                                     Baoulé
#> 121               bcl                              Central Bikol
#> 122               bdr                           West Coast Bajau
#> 123                be                                 Belarusian
#> 124         be-tarask      Belarusian (Taraškievica orthography)
#> 125          be-x-old      Belarusian (Taraškievica orthography)
#> 126               bej                                       Beja
#> 127               bem                                      Bemba
#> 128               ber                           Berber languages
#> 129               bew                                     Betawi
#> 130               bez                                       Bena
#> 131               bfa                                       Bari
#> 132               bfd                                      Bafut
#> 133               bfi                      British Sign Language
#> 134               bfq                                     Badaga
#> 135               bft                                      Balti
#> 136          bft-tibt                     Balti (Tibetan script)
#> 137               bfw                                      Bonda
#> 138               bfz                              Mahasu Pahari
#> 139          bfz-deva          Mahasu Pahari (Devanagari script)
#> 140          bfz-takr               Mahasu Pahari (Takri script)
#> 141                bg                                  Bulgarian
#> 142               bgc                                   Haryanvi
#> 143          bgc-arab                   Haryanvi (Arabic script)
#> 144          bgc-deva               Haryanvi (Devanagari script)
#> 145               bgn                            Western Balochi
#> 146               bgp                            Eastern Balochi
#> 147               bgq                                      Bagri
#> 148          bgq-arab                      Bagri (Arabic script)
#> 149          bgq-deva                  Bagri (Devanagari script)
#> 150                bh                                   Bhojpuri
#> 151               bha                                     Bharia
#> 152               bhd                                 Bhadrawahi
#> 153          bhd-deva             Bhadrawahi (Devanagari script)
#> 154          bhd-takr                  Bhadrawahi (Takri script)
#> 155               bho                                   Bhojpuri
#> 156                bi                                    Bislama
#> 157               bik                                      Bikol
#> 158               bin                                       Bini
#> 159               bjn                                     Banjar
#> 160               bkc                                       Baka
#> 161               bkh                                     Bakoko
#> 162               bkm                                        Kom
#> 163               bkn                                    Bukitan
#> 164               bla                                    Siksiká
#> 165               blc                                     Nuxalk
#> 166               blk                                       Pa'O
#> 167               blo                                       Anii
#> 168               blt                                    Tai Dam
#> 169                bm                                    Bambara
#> 170                bn                                     Bangla
#> 171               bnb                                     Bookan
#> 172               bnn                                      Bunun
#> 173               bnt                            Bantu languages
#> 174               bny                                    Bintulu
#> 175                bo                                    Tibetan
#> 176               bol                                       Bole
#> 177               bom                                      Berom
#> 178               bpy                                Bishnupriya
#> 179               bqi                                  Bakhtiari
#> 180               bqz                                      Mka'a
#> 181                br                                     Breton
#> 182               bra                                       Braj
#> 183               brh                                     Brahui
#> 184          brh-latn                      Brahui (Latin script)
#> 185               brx                                       Bodo
#> 186                bs                                    Bosnian
#> 187               bse                                      Wushi
#> 188               bsk                                 Burushaski
#> 189               bss                                     Akoose
#> 190               btd                                Batak Dairi
#> 191               bth                                     Biatah
#> 192               btk                            Batak languages
#> 193               btm                           Batak Mandailing
#> 194               bto                            Rinconada Bikol
#> 195               bts                           Batak Simalungun
#> 196               btx                                 Batak Karo
#> 197               btz                           Batak Alas-Kluet
#> 198               bua                                     Buriat
#> 199               bug                                   Buginese
#> 200          bug-bugi                 Buginese (Buginese script)
#> 201               bum                                       Bulu
#> 202               bvb                                       Bube
#> 203               bwr                                 Bura-Pabir
#> 204               bxr                              Russia Buriat
#> 205               byn                                       Blin
#> 206               byv                                    Medumba
#> 207               bzj                               Belize Kriol
#> 208               bzs                    Brazilian Sign Language
#> 209                ca                                    Catalan
#> 210               cad                                      Caddo
#> 211               cai                     Mesoamerican languages
#> 212               cak                                  Kaqchikel
#> 213               cal                                 Carolinian
#> 214               car                                      Carib
#> 215               cau                        Caucasian languages
#> 216               cay                                     Cayuga
#> 217               cbk                                  Chavacano
#> 218           cbk-zam                                  Chavacano
#> 219               cch                                      Atsam
#> 220               ccp                                     Chakma
#> 221          ccp-beng                    Chakma (Bengali script)
#> 222               cdo                                    Mindong
#> 223          cdo-hani                       Mindong (Han script)
#> 224          cdo-hant           Mindong (Traditional Han script)
#> 225          cdo-latn                     Mindong (Latin script)
#> 226          cdz-beng                      Koda (Bengali script)
#> 227                ce                                    Chechen
#> 228               ceb                                    Cebuano
#> 229               cel                           Celtic languages
#> 230               cgg                                      Chiga
#> 231                ch                                   Chamorro
#> 232               chb                                    Chibcha
#> 233               chg                                   Chagatai
#> 234               chk                                   Chuukese
#> 235               chm                                       Mari
#> 236               chn                             Chinook Jargon
#> 237               cho                                    Choctaw
#> 238               chp                                  Chipewyan
#> 239               chr                                   Cherokee
#> 240               chy                                   Cheyenne
#> 241               cic                                  Chickasaw
#> 242               ciw                                   Chippewa
#> 243               cja                               Western Cham
#> 244          cja-arab               Western Cham (Arabic script)
#> 245          cja-cham                 Western Cham (Cham script)
#> 246          cja-latn                Western Cham (Latin script)
#> 247               cjm                               Eastern Cham
#> 248          cjm-arab               Eastern Cham (Arabic script)
#> 249          cjm-cham                 Eastern Cham (Cham script)
#> 250          cjm-latn                Eastern Cham (Latin script)
#> 251               cjy                                        Jin
#> 252          cjy-hans                Jin (Simplified Han script)
#> 253          cjy-hant               Jin (Traditional Han script)
#> 254               ckb                            Central Kurdish
#> 255          ckb-arab            Central Kurdish (Arabic script)
#> 256          ckb-latn             Central Kurdish (Latin script)
#> 257               cko                                      Anufo
#> 258               ckt                                    Chukchi
#> 259               ckv                                    Kavalan
#> 260               clc                                  Chilcotin
#> 261               cmc                           Chamic languages
#> 262               cmg                        Classical Mongolian
#> 263               cnh                                 Hakha-Chin
#> 264               cnr                                Montenegrin
#> 265          cnr-cyrl              Montenegrin (Cyrillic script)
#> 266          cnr-latn                 Montenegrin (Latin script)
#> 267               cnx                             Middle Cornish
#> 268                co                                   Corsican
#> 269               coa                                Cocos Malay
#> 270               cop                                     Coptic
#> 271               cpe             English-based creole languages
#> 272               cpf              French-based creole languages
#> 273               cpp          Portuguese-based creole languages
#> 274               cps                                   Capiznon
#> 275               cpx                                     Puxian
#> 276          cpx-hans             Puxian (Simplified Han script)
#> 277          cpx-hant            Puxian (Traditional Han script)
#> 278          cpx-latn                      Puxian (Latin script)
#> 279                cr                                       Cree
#> 280           cr-cans       Cree (Canadian Aboriginal syllabics)
#> 281           cr-latn                        Cree (Latin script)
#> 282               crb                               Island Carib
#> 283               crg                                     Michif
#> 284               crh                              Crimean Tatar
#> 285          crh-cyrl            Crimean Tatar (Cyrillic script)
#> 286          crh-latn               Crimean Tatar (Latin script)
#> 287            crh-ro                             Dobrujan Tatar
#> 288               crj                         Southern East Cree
#> 289               crk                                Plains Cree
#> 290               crl                         Northern East Cree
#> 291               crm                                 Moose Cree
#> 292               crp                        creoles and pidgins
#> 293               crr                        Carolina Algonquian
#> 294               crs                      Seselwa Creole French
#> 295                cs                                      Czech
#> 296               csb                                  Kashubian
#> 297               csw                                Swampy Cree
#> 298               ctg                               Chittagonian
#> 299                cu                              Church Slavic
#> 300               cus                         Cushitic languages
#> 301                cv                                    Chuvash
#> 302                cy                                      Welsh
#> 303                da                                     Danish
#> 304               dag                                    Dagbani
#> 305               dak                                     Dakota
#> 306               dar                                     Dargwa
#> 307               dav                                      Taita
#> 308               day                       Land Dayak languages
#> 309               dbj                                     Idaʼan
#> 310               ddn                                      Dendi
#> 311                de                                     German
#> 312           de-1901           German (traditional orthography)
#> 313             de-at                            Austrian German
#> 314             de-ch                          Swiss High German
#> 315         de-formal                    German (formal address)
#> 316               del                                   Delaware
#> 317               den                                      Slave
#> 318               dga                           Southern Dagaare
#> 319               dgr                                     Dogrib
#> 320               din                                      Dinka
#> 321               diq                                      Dimli
#> 322               dje                                      Zarma
#> 323               dkr                                     Kuijau
#> 324               dlg                                     Dolgan
#> 325               dmg                         Upper Kinabatangan
#> 326               dmv                                     Dumpas
#> 327               doi                                      Dogri
#> 328          doi-arab                      Dogri (Arabic script)
#> 329          doi-deva                  Dogri (Devanagari script)
#> 330          doi-dogr                       Dogri (Dogra script)
#> 331               dpp                                      Papar
#> 332               dra                        Dravidian languages
#> 333               drg                                     Rungus
#> 334               dro                                  Daro-Matu
#> 335               dru                                      Rukai
#> 336               dsb                              Lower Sorbian
#> 337               dso                                     Desiya
#> 338               dtb                            Eastern Kadazan
#> 339               dtp                              Central Dusun
#> 340               dtr                                      Lotud
#> 341               dty                                     Doteli
#> 342               dua                                      Duala
#> 343               duf                                     Dumbea
#> 344               dum                               Middle Dutch
#> 345                dv                                     Divehi
#> 346               dyo                                 Jola-Fonyi
#> 347               dyu                                      Dyula
#> 348                dz                                   Dzongkha
#> 349               dzg                                     Dazaga
#> 350               ebu                                       Embu
#> 351                ee                                        Ewe
#> 352               efi                                       Efik
#> 353               egl                         Emiliano-Romagnolo
#> 354               egy                           Ancient Egyptian
#> 355               eka                                     Ekajuk
#> 356               ekp                                     Ekpeye
#> 357                el                                      Greek
#> 358             el-cy                              Cypriot Greek
#> 359               elm                                      Eleme
#> 360               elx                                    Elamite
#> 361               eml                         Emiliano-Romagnolo
#> 362                en                                    English
#> 363             en-au                         Australian English
#> 364             en-ca                           Canadian English
#> 365             en-gb                            British English
#> 366             en-in                             Indian English
#> 367             en-jm                           Jamaican English
#> 368             en-nz                        New Zealand English
#> 369         en-simple                             Simple English
#> 370             en-uk                            British English
#> 371             en-us                           American English
#> 372               enm                             Middle English
#> 373                eo                                  Esperanto
#> 374       eo-hsistemo           Esperanto (h-system orthography)
#> 375       eo-xsistemo           Esperanto (x-system orthography)
#> 376                es                                    Spanish
#> 377            es-419                     Latin American Spanish
#> 378             es-es                           European Spanish
#> 379         es-formal                   Spanish (formal address)
#> 380             es-mx                            Mexican Spanish
#> 381             es-ni                        Spanish (Nicaragua)
#> 382               ess                     Central Siberian Yupik
#> 383               esu                              Central Yupik
#> 384                et                                   Estonian
#> 385               eto                                       Eton
#> 386               ett                                   Etruscan
#> 387               etu                                    Ejagham
#> 388                eu                                     Basque
#> 389               ewo                                     Ewondo
#> 390               ext                               Extremaduran
#> 391               eya                                       Eyak
#> 392                fa                                    Persian
#> 393             fa-af                                       Dari
#> 394               fab                          Annobonese Creole
#> 395               fan                                       Fang
#> 396               fat                                      Fanti
#> 397               fax                                       Fala
#> 398               fay                                  Kuhmareyi
#> 399                ff                                       Fula
#> 400                fi                                    Finnish
#> 401               fil                                   Filipino
#> 402               fit                         Tornedalen Finnish
#> 403               fiu                      Finno-Ugric languages
#> 404           fiu-vro                                       Võro
#> 405                fj                                     Fijian
#> 406               fkv                                     Kvensk
#> 407               fmp                                     Fe'Fe'
#> 408                fo                                    Faroese
#> 409               fon                                        Fon
#> 410               fos                                     Siraya
#> 411                fr                                     French
#> 412             fr-be                             Belgian French
#> 413             fr-ca                            Canadian French
#> 414             fr-ch                               Swiss French
#> 415               frc                               Cajun French
#> 416               frk                                   Frankish
#> 417               frm                              Middle French
#> 418               fro                                 Old French
#> 419               frp                                    Arpitan
#> 420               frr                           Northern Frisian
#> 421               frs                            Eastern Frisian
#> 422               fsl                       French Sign Language
#> 423               fud                                    Futunan
#> 424               fuf                                      Pular
#> 425               fur                                   Friulian
#> 426               fvr                                        Fur
#> 427                fy                            Western Frisian
#> 428                ga                                      Irish
#> 429               gaa                                         Ga
#> 430               gag                                     Gagauz
#> 431               gah                                    Alekano
#> 432               gan                                        Gan
#> 433          gan-hans                Gan (Simplified Han script)
#> 434          gan-hant               Gan (Traditional Han script)
#> 435               gay                                       Gayo
#> 436               gba                                      Gbaya
#> 437               gbb                                   Kaytetye
#> 438               gbk                                      Gaddi
#> 439          gbk-deva                  Gaddi (Devanagari script)
#> 440          gbk-takr                       Gaddi (Takri script)
#> 441               gbm                                   Garhwali
#> 442               gbz                           Zoroastrian Dari
#> 443               gcf                        Guadeloupean Creole
#> 444               gcr                             Guianan Creole
#> 445                gd                            Scottish Gaelic
#> 446               gem                         Germanic languages
#> 447               gez                                       Geez
#> 448               gil                                 Gilbertese
#> 449               gju                                     Gujari
#> 450          gju-arab                     Gujari (Arabic script)
#> 451          gju-deva                 Gujari (Devanagari script)
#> 452                gl                                   Galician
#> 453               gld                                      Nanai
#> 454               glh                           Northwest Pashai
#> 455               glk                                     Gilaki
#> 456               gmh                         Middle High German
#> 457               gml                          Middle Low German
#> 458               gmy                            Mycenaean Greek
#> 459                gn                                    Guarani
#> 460               gnq                                      Ganaʼ
#> 461               goh                            Old High German
#> 462               gom                               Goan Konkani
#> 463          gom-deva           Goan Konkani (Devanagari script)
#> 464          gom-latn                Goan Konkani (Latin script)
#> 465               gon                                      Gondi
#> 466               gor                                  Gorontalo
#> 467               got                                     Gothic
#> 468               gpe                            Ghanaian Pidgin
#> 469               grb                                      Grebo
#> 470               grc                              Ancient Greek
#> 471               gsg                       German Sign Language
#> 472               gsw                                  Alemannic
#> 473            gsw-fr                                   Alsatian
#> 474                gu                                   Gujarati
#> 475               guc                                      Wayuu
#> 476               gum                                  Guambiano
#> 477               gur                                     Frafra
#> 478               guw                                        Gun
#> 479               guz                                      Gusii
#> 480                gv                                       Manx
#> 481               gwi                                   Gwichʼin
#> 482               gya                                      Gbaya
#> 483                ha                                      Hausa
#> 484           ha-arab                      Hausa (Arabic script)
#> 485           ha-latn                       Hausa (Latin script)
#> 486             ha-ne                              Hausa (Niger)
#> 487               hac                                     Gurani
#> 488               hai                                      Haida
#> 489               hak                              Hakka Chinese
#> 490          hak-hans              Hakka (Simplified Han script)
#> 491          hak-hant             Hakka (Traditional Han script)
#> 492          hak-latn                       Hakka (Latin script)
#> 493               hav                                       Havu
#> 494               haw                                   Hawaiian
#> 495               hax                             Southern Haida
#> 496               haz                                   Hazaragi
#> 497               hbo                            Biblical Hebrew
#> 498                he                                     Hebrew
#> 499               hea                     Northern Qiandong Miao
#> 500                hi                                      Hindi
#> 501           hi-kthi                      Hindi (Kaithi script)
#> 502           hi-latn                              Hindi (Latin)
#> 503               hif                                 Fiji Hindi
#> 504          hif-deva             Fiji Hindi (Devanagari script)
#> 505          hif-latn                  Fiji Hindi (Latin script)
#> 506               hil                                 Hiligaynon
#> 507               him                             Western Pahari
#> 508               hit                                    Hittite
#> 509          hit-latn                     Hittite (Latin script)
#> 510          hit-xsux                 Hittite (Cuneiform script)
#> 511               hke                                      Hunde
#> 512               hmn                                      Hmong
#> 513               hne                              Chhattisgarhi
#> 514               hnj                                 Hmong Njua
#> 515               hno                            Northern Hindko
#> 516                ho                                  Hiri Motu
#> 517               hoc                                         Ho
#> 518          hoc-latn                          Ho (Latin script)
#> 519                hr                                   Croatian
#> 520               hrx                                    Hunsrik
#> 521               hsb                              Upper Sorbian
#> 522               hsn                                      Xiang
#> 523          hsn-hans              Xiang (Simplified Han script)
#> 524          hsn-hant             Xiang (Traditional Han script)
#> 525                ht                             Haitian Creole
#> 526               hts                                      Hadza
#> 527                hu                                  Hungarian
#> 528         hu-formal                 Hungarian (formal address)
#> 529               hup                                       Hupa
#> 530               hur                                 Halkomelem
#> 531                hy                                   Armenian
#> 532               hyw                           Western Armenian
#> 533                hz                                     Herero
#> 534                ia                                Interlingua
#> 535               iba                                       Iban
#> 536               ibb                                     Ibibio
#> 537                id                                 Indonesian
#> 538                ie                                Interlingue
#> 539               ifu                             Mayoyao Ifugao
#> 540                ig                                       Igbo
#> 541               igb                                      Ebira
#> 542               igl                                      Igala
#> 543                ii                                 Sichuan Yi
#> 544               ijo                             Ijaw languages
#> 545                ik                                    Inupiaq
#> 546          ike-cans    Eastern Canadian (Aboriginal syllabics)
#> 547          ike-latn            Eastern Canadian (Latin script)
#> 548               ikt                 Western Canadian Inuktitut
#> 549               ilo                                      Iloko
#> 550               inc                       Indo-Aryan languages
#> 551               ine                    Indo-European languages
#> 552               inh                                     Ingush
#> 553                io                                        Ido
#> 554               ira                          Iranian languages
#> 555               iro                        Iroquoian languages
#> 556                is                                  Icelandic
#> 557               ish                                       Esan
#> 558          isk-arab                 Ishkashimi (Arabic script)
#> 559          isk-cyrl               Ishkashimi (Cyrillic script)
#> 560          isk-latn                  Ishkashimi (Latin script)
#> 561               ist                                    Istriot
#> 562               isu                                        Isu
#> 563               isv                            medžuslovjansky
#> 564          isv-cyrl              Interslavic (Cyrillic script)
#> 565          isv-latn                 Interslavic (Latin script)
#> 566                it                                    Italian
#> 567                iu                                  Inuktitut
#> 568               ivb                                     Ibatan
#> 569               izh                                    Ingrian
#> 570                ja                                   Japanese
#> 571           ja-hani                    Japanese (Kanji script)
#> 572           ja-hira                 Japanese (Hiragana script)
#> 573           ja-hrkt                     Japanese (Kana script)
#> 574           ja-kana                 Japanese (Katakana script)
#> 575               jac                                     Popti'
#> 576               jak                                      Jakun
#> 577               jam                    Jamaican Creole English
#> 578               jax                                Jambi Malay
#> 579               jbo                                     Lojban
#> 580               jdt                                  Judeo-Tat
#> 581          jdt-cyrl                Judeo-Tat (Cyrillic script)
#> 582               jgo                                     Ngomba
#> 583               jje                                       Jeju
#> 584               jmc                                    Machame
#> 585               jpr                              Judeo-Persian
#> 586               jrb                               Judeo-Arabic
#> 587               juk                                      Wapan
#> 588               jut                                     Jutish
#> 589                jv                                   Javanese
#> 590           jv-java                 Javanese (Javanese script)
#> 591                ka                                   Georgian
#> 592               kaa                                Kara-Kalpak
#> 593               kab                                     Kabyle
#> 594               kac                                     Kachin
#> 595               kag                                    Kajaman
#> 596               kai                                   Karekare
#> 597               kaj                                        Jju
#> 598               kam                                      Kamba
#> 599               kar                          Karenic languages
#> 600               kaw                                       Kawi
#> 601               kbd                                  Kabardian
#> 602          kbd-cyrl                Kabardian (Cyrillic script)
#> 603          kbd-latn                   Kabardian (Latin script)
#> 604               kbl                                    Kanembu
#> 605               kbp                                     Kabiye
#> 606               kcg                                       Tyap
#> 607               kck                                    Kalanga
#> 608               kde                                    Makonde
#> 609               kea                        Cape Verdean Creole
#> 610               kek                                   Qʼeqchiʼ
#> 611               ken                                    Kenyang
#> 612               ker                                       Kera
#> 613               kfo                                       Koro
#> 614               kfr                                     Kutchi
#> 615                kg                                      Kongo
#> 616               kge                                   Komering
#> 617          kge-arab                   Komering (Arabic script)
#> 618               kgg                                    Kusunda
#> 619               kgp                                   Kaingang
#> 620               kha                                      Khasi
#> 621               khi                          Khoisan languages
#> 622               kho                                  Khotanese
#> 623               khq                               Koyra Chiini
#> 624               khw                                     Khowar
#> 625                ki                                     Kikuyu
#> 626               kip                                Sheshi Kham
#> 627               kiu                                  Kirmanjki
#> 628                kj                                   Kuanyama
#> 629               kjh                                     Khakas
#> 630               kjp                                Eastern Pwo
#> 631                kk                                     Kazakh
#> 632           kk-arab                     Kazakh (Arabic script)
#> 633             kk-cn                             Kazakh (China)
#> 634           kk-cyrl                   Kazakh (Cyrillic script)
#> 635             kk-kz                        Kazakh (Kazakhstan)
#> 636           kk-latn                      Kazakh (Latin script)
#> 637             kk-tr                            Kazakh (Turkey)
#> 638               kkj                                       Kako
#> 639                kl                                Kalaallisut
#> 640               kld                                 Gamilaraay
#> 641               kln                                   Kalenjin
#> 642               kls                                    Kalasha
#> 643          kls-arab                    Kalasha (Arabic script)
#> 644          kls-latn                     Kalasha (Latin script)
#> 645                km                                      Khmer
#> 646               kmb                                   Kimbundu
#> 647               kmr                           Northern Kurdish
#> 648          kmr-arab           Northern Kurdish (Arabic script)
#> 649          kmr-latn            Northern Kurdish (Latin script)
#> 650               kmz                           Khorasani Turkic
#> 651                kn                                    Kannada
#> 652               knc                             Central Kanuri
#> 653               kne                                  Kankanaey
#> 654               knn                      Maharashtrian Konkani
#> 655               knq                                     Kintaq
#> 656                ko                                     Korean
#> 657             ko-cn                             Korean (China)
#> 658           ko-hani                      Korean (Hanja script)
#> 659           ko-kore                      Korean (mixed script)
#> 660             ko-kp                       Korean (North Korea)
#> 661               koi                               Komi-Permyak
#> 662               kok                                    Konkani
#> 663               kos                                   Kosraean
#> 664               koy                                    Koyukon
#> 665               kpe                                     Kpelle
#> 666               kqr                                 Kimaragang
#> 667               kqt                        Klias River Kadazan
#> 668               kqv                                     Okolod
#> 669                kr                                     Kanuri
#> 670               krc                            Karachay-Balkar
#> 671               kri                                       Krio
#> 672               krj                                  Kinaray-a
#> 673               krl                                   Karelian
#> 674               kro                              Kru languages
#> 675               kru                                     Kurukh
#> 676                ks                                   Kashmiri
#> 677           ks-arab                   Kashmiri (Arabic script)
#> 678           ks-deva               Kashmiri (Devanagari script)
#> 679               ksb                                   Shambala
#> 680               ksf                                      Bafia
#> 681               ksh                                  Colognian
#> 682               ksw                                S'gaw Karen
#> 683          ksy-beng               Kharia Thar (Bengali script)
#> 684                ku                                    Kurdish
#> 685           ku-arab                    Kurdish (Arabic script)
#> 686           ku-latn                     Kurdish (Latin script)
#> 687               kum                                      Kumyk
#> 688               kus                                     Kusaal
#> 689               kut                                    Kutenai
#> 690                kv                                       Komi
#> 691               kve                                  Kalabakan
#> 692                kw                                    Cornish
#> 693               kwk                                  Kwakʼwala
#> 694               kxd                               Brunei Malay
#> 695               kxi                             Keningau Murut
#> 696               kxn                                    Kanowit
#> 697               kxv                                       Kuvi
#> 698                ky                                     Kyrgyz
#> 699          kyw-beng                   Kurmali (Bengali script)
#> 700          kyw-deva                Kurmali (Devanagari script)
#> 701                la                                      Latin
#> 702               lad                                     Ladino
#> 703          lad-hebr                     Ladino (Hebrew script)
#> 704          lad-latn                      Ladino (Latin script)
#> 705               lag                                      Langi
#> 706               lah                            Western Panjabi
#> 707               laj                                      Lango
#> 708               lam                                      Lamba
#> 709                lb                              Luxembourgish
#> 710               lbe                                        Lak
#> 711               lcm                                     Tungag
#> 712               ldn                                     Láadan
#> 713               lem                                   Nomaande
#> 714               lez                                   Lezghian
#> 715               lfn                         Lingua Franca Nova
#> 716                lg                                      Ganda
#> 717                li                                 Limburgish
#> 718             li-be                         Belgian Limburgish
#> 719             li-nl                           Dutch Limburgish
#> 720               lij                                   Ligurian
#> 721            lij-mc                                 Monégasque
#> 722               lil                                   Lillooet
#> 723               liv                                   Livonian
#> 724               ljp                                Lampung Api
#> 725               lki                                       Laki
#> 726               lkt                                     Lakota
#> 727               lld                                      Ladin
#> 728               lmn                                    Lambadi
#> 729               lmo                                    Lombard
#> 730                ln                                    Lingala
#> 731               lns                                    Lamnso'
#> 732                lo                                        Lao
#> 733               lol                                      Mongo
#> 734               lou                           Louisiana Creole
#> 735               loz                                       Lozi
#> 736               lrc                              Northern Luri
#> 737               lsm                                     Saamia
#> 738                lt                                 Lithuanian
#> 739               ltg                                  Latgalian
#> 740                lu                               Luba-Katanga
#> 741               lua                                 Luba-Lulua
#> 742               lud                                      Ludic
#> 743               lui                                    Luiseno
#> 744               lun                                      Lunda
#> 745               luo                                        Luo
#> 746               lus                                       Mizo
#> 747               lut                                Lushootseed
#> 748               luy                                      Luyia
#> 749               luz                              Southern Luri
#> 750                lv                                    Latvian
#> 751               lzh                           Literary Chinese
#> 752               lzz                                        Laz
#> 753               mad                                   Madurese
#> 754               maf                                       Mafa
#> 755               mag                                     Magahi
#> 756               mai                                   Maithili
#> 757               mak                                    Makasar
#> 758          mak-bugi                  Makasar (Buginese script)
#> 759               man                                   Mandingo
#> 760               map                     Austronesian languages
#> 761           map-bms                                 Banyumasan
#> 762               mas                                      Masai
#> 763               maw                                   Mampruli
#> 764               mcn                                      Massa
#> 765               mcp                                       Maka
#> 766               mde                                       Maba
#> 767               mdf                                     Moksha
#> 768               mdh                               Maguindanaon
#> 769               mdr                                     Mandar
#> 770               men                                      Mende
#> 771               mer                                       Meru
#> 772               mey                                 Hassaniyya
#> 773               mfa                     Kelantan-Pattani Malay
#> 774               mfe                                   Morisyen
#> 775                mg                                   Malagasy
#> 776               mga                               Middle Irish
#> 777               mgh                             Makhuwa-Meetto
#> 778               mgo                                      Metaʼ
#> 779                mh                                Marshallese
#> 780               mhk                                    Mungaka
#> 781               mhn                                    Mòcheno
#> 782               mhr                               Eastern Mari
#> 783                mi                                      Māori
#> 784               mic                                    Mi'kmaw
#> 785               mid                                    Mandaic
#> 786               min                                Minangkabau
#> 787               miq                                    Miskito
#> 788               mis                       unsupported language
#> 789               mix                                     Mixtec
#> 790          mjx-beng                    Mahali (Bengali script)
#> 791                mk                                 Macedonian
#> 792               mkh                                  Mon-Khmer
#> 793                ml                                  Malayalam
#> 794                mn                                  Mongolian
#> 795           mn-cyrl                Mongolian (Cyrillic script)
#> 796           mn-mong               Mongolian (Mongolian script)
#> 797               mnc                                     Manchu
#> 798          mnc-latn                      Manchu (Latin script)
#> 799          mnc-mong                  Manchu (Mongolian script)
#> 800               mni                                   Manipuri
#> 801          mni-beng                  Manipuri (Bengali script)
#> 802               mnj                                      Munji
#> 803               mno                           Manobo languages
#> 804               mnq                                     Minriq
#> 805               mns                                      Mansi
#> 806               mnw                                        Mon
#> 807                mo                                   Moldovan
#> 808               moe                                 Innu-aimun
#> 809               moh                                     Mohawk
#> 810               mos                                      Mossi
#> 811                mr                                    Marathi
#> 812           mr-modi                      Marathi (Modi script)
#> 813               mrh                                       Mara
#> 814               mrj                               Western Mari
#> 815               mrt                             Marghi Central
#> 816               mrv                                  Mangareva
#> 817                ms                                      Malay
#> 818           ms-arab                        Malay (Jawi script)
#> 819               msi                                Sabah Malay
#> 820                mt                                    Maltese
#> 821               mua                                    Mundang
#> 822               mui                                       Musi
#> 823               mul                         multiple languages
#> 824               mun                            Munda languages
#> 825               mus                                   Muscogee
#> 826               mvf                       Peripheral Mongolian
#> 827               mvi                                     Miyako
#> 828          mvi-hira                   Miyako (Hiragana script)
#> 829               mvv                                      Tagol
#> 830               mwl                                  Mirandese
#> 831               mwr                                    Marwari
#> 832               mwv                                   Mentawai
#> 833               mww                                  Hmong Daw
#> 834          mww-latn                   Hmong Daw (Latin script)
#> 835                my                                    Burmese
#> 836               mye                                      Myene
#> 837               myn                            Mayan languages
#> 838               myv                                      Erzya
#> 839               mzn                                Mazanderani
#> 840                na                                      Nauru
#> 841               nah                                    Nahuatl
#> 842               nai      Indigenous languages of North America
#> 843               nan                                     Minnan
#> 844          nan-hani                        Minnan (Han script)
#> 845          nan-hans             Minnan (Simplified Han script)
#> 846          nan-hant            Minnan (Traditional Han script)
#> 847  nan-latn-pehoeji                         Minnan (Pe̍h-ōe-jī)
#> 848    nan-latn-tailo                            Minnan (Tâi-lô)
#> 849               nap                                 Neapolitan
#> 850               naq                                       Nama
#> 851                nb                           Norwegian Bokmål
#> 852                nd                              North Ndebele
#> 853               nds                                 Low German
#> 854            nds-nl                                  Low Saxon
#> 855                ne                                     Nepali
#> 856               new                                     Newari
#> 857                ng                                     Ndonga
#> 858               nge                                     Ngémba
#> 859               nia                                       Nias
#> 860               nic                      Niger–Congo languages
#> 861               nit                        Southeastern Kolami
#> 862               niu                                     Niuean
#> 863               njo                                    Ao Naga
#> 864                nl                                      Dutch
#> 865             nl-be                                    Flemish
#> 866       nl-informal                   Dutch (informal address)
#> 867               nla                                   Ngombala
#> 868               nmg                                     Kwasio
#> 869               nmz                                      Nawdm
#> 870                nn                          Norwegian Nynorsk
#> 871       nn-hognorsk                         Norwegian Høgnorsk
#> 872               nnh                                  Ngiemboon
#> 873               nnz                                   Nda'Nda'
#> 874                no                                  Norwegian
#> 875               nod                              Northern Thai
#> 876          nod-thai                Northern Thai (Thai script)
#> 877               nog                                      Nogai
#> 878               non                                  Old Norse
#> 879          non-runr                   Old Norse (Runic script)
#> 880               nov                                     Novial
#> 881               nqo                                       N’Ko
#> 882                nr                              South Ndebele
#> 883            nrf-gg                                Guernésiais
#> 884            nrf-je                                   Jèrriais
#> 885               nrm                                     Norman
#> 886               nsk                                    Naskapi
#> 887               nsl                    Norwegian Sign Language
#> 888               nso                             Northern Sotho
#> 889               ntd                             Sesayap Tidung
#> 890               nub                           Nubian languages
#> 891               nup                                       Nupe
#> 892               nus                                       Nuer
#> 893                nv                                     Navajo
#> 894               nwc                           Classical Newari
#> 895               nxm                                   Numidian
#> 896                ny                                     Nyanja
#> 897               nym                                   Nyamwezi
#> 898               nyn                                   Nyankole
#> 899               nyo                                      Nyoro
#> 900               nys                                    Nyungar
#> 901               nzi                                      Nzima
#> 902               obt                                 Old Breton
#> 903                oc                                    Occitan
#> 904               oco                                Old Cornish
#> 905               odt                                  Old Dutch
#> 906               ofs                                Old Frisian
#> 907                oj                                     Ojibwa
#> 908               ojb                        Northwestern Ojibwa
#> 909               ojc                             Central Ojibwa
#> 910               ojp                               Old Japanese
#> 911          ojp-hani                Old Japanese (Kanji script)
#> 912          ojp-hira             Old Japanese (Hiragana script)
#> 913               ojs                                   Oji-Cree
#> 914               ojw                             Western Ojibwa
#> 915               oka                                   Okanagan
#> 916               olo                             Livvi-Karelian
#> 917                om                                      Oromo
#> 918               oma                                Omaha-Ponca
#> 919               ood                                    O'odham
#> 920                or                                       Odia
#> 921                os                                    Ossetic
#> 922               osa                                      Osage
#> 923          osa-latn                       Osage (Latin script)
#> 924               osi                                      Osing
#> 925               osx                                  Old Saxon
#> 926               ota                            Ottoman Turkish
#> 927               otk                                Old Turkish
#> 928               oto                          Otomian languages
#> 929               ovd                                  Elfdalian
#> 930               owl                                  Old Welsh
#> 931                pa                                    Punjabi
#> 932           pa-guru                  Punjabi (Gurmukhi script)
#> 933               paa                           Papuan languages
#> 934               pag                                 Pangasinan
#> 935               pal                                    Pahlavi
#> 936          pal-phli     Pahlavi (Inscriptional Pahlavi script)
#> 937          pal-phlp           Pahlavi (Psalter Pahlavi script)
#> 938          pal-phlv              Pahlavi (Book Pahlavi script)
#> 939               pam                                   Pampanga
#> 940               pao                            Northern Paiute
#> 941               pap                                 Papiamento
#> 942            pap-aw                         Papiamento (Aruba)
#> 943               paq                                      Parya
#> 944               pau                                    Palauan
#> 945               pbb                                       Páez
#> 946               pcd                                     Picard
#> 947               pcm                            Nigerian Pidgin
#> 948               pdc                        Pennsylvania German
#> 949               pdt                               Plautdietsch
#> 950               peo                                Old Persian
#> 951               pfl                            Palatine German
#> 952               pgd                                   Gāndhārī
#> 953          pgd-arab                   Gāndhārī (Arabic script)
#> 954          pgd-deva               Gāndhārī (Devanagari script)
#> 955          pgd-khar               Gāndhārī (Kharoshthi script)
#> 956               pgl                            Primitive Irish
#> 957               phi                       Philippine languages
#> 958               phl                                     Palula
#> 959               phn                                 Phoenician
#> 960          phn-latn                  Phoenician (Latin script)
#> 961          phn-phnx             Phoenician (Phoenician script)
#> 962               phr                             Pahari-Potwari
#> 963                pi                                       Pali
#> 964           pi-sidd                      Pali (Siddham script)
#> 965               pih                           Pitcairn-Norfolk
#> 966               pis                                      Pijin
#> 967               pjt                             Pitjantjatjara
#> 968               pkc                                    Paekche
#> 969               pko                                     Pökoot
#> 970               pks                     Pakistan Sign Language
#> 971                pl                                     Polish
#> 972               plv                         Southwest Palawano
#> 973               plw                    Brooke's Point Palawano
#> 974               pms                                Piedmontese
#> 975               pnb                            Western Punjabi
#> 976               pnt                                     Pontic
#> 977               pon                                  Pohnpeian
#> 978               pov                       Upper Guinea Crioulo
#> 979               ppl                                      Nawat
#> 980               ppu                              Papora-Hoanya
#> 981               pqm                     Maliseet-Passamaquoddy
#> 982               pra                                    Prakrit
#> 983               prc                                    Parachi
#> 984               prg                                   Prussian
#> 985               pro                              Old Provençal
#> 986               prs                                       Dari
#> 987                ps                                     Pashto
#> 988             ps-af                       Pashto (Afghanistan)
#> 989             ps-pk                          Pashto (Pakistan)
#> 990               psh                           Southwest Pashai
#> 991               psi                           Southeast Pashai
#> 992               psu                          Sauraseni Prākrit
#> 993          psu-arab          Sauraseni Prākrit (Arabic script)
#> 994          psu-brah          Sauraseni Prākrit (Brahmi script)
#> 995          psu-deva      Sauraseni Prākrit (Devanagari script)
#> 996          psu-guru        Sauraseni Prākrit (Gurmukhi script)
#> 997                pt                                 Portuguese
#> 998         pt-ao1990   Portuguese (1990 Orthographic Agreement)
#> 999             pt-br                       Brazilian Portuguese
#> 1000      pt-colb1945   Portuguese (1945 Orthographic Agreement)
#> 1001            pt-pt                        European Portuguese
#> 1002              pwn                                     Paiwan
#> 1003              pwo                                Western Pwo
#> 1004              pyu                                     Puyuma
#> 1005               qu                                    Quechua
#> 1006              quc                                    Kʼicheʼ
#> 1007              qug                Chimborazo Highland Quichua
#> 1008              qwh                     Huaylas Ancash Quechua
#> 1009              qxp                               Puno Quechua
#> 1010              qxq                                    Qashqai
#> 1011              qya                                     Quenya
#> 1012              rag                                    Logooli
#> 1013              rah                                      Rabha
#> 1014              raj                                 Rajasthani
#> 1015              rap                                    Rapanui
#> 1016              rar                                 Rarotongan
#> 1017              rcf                      Réunion Creole French
#> 1018              rej                                     Rejang
#> 1019              rgn                                   Romagnol
#> 1020              rhg                                   Rohingya
#> 1021         rhg-arab                   Rohingya (Arabic script)
#> 1022         rhg-rohg          Rohingya (Hanifi Rohingya script)
#> 1023              rif                                    Riffian
#> 1024              rki                                  Arakanese
#> 1025              rkt                                   Rangpuri
#> 1026               rm                                    Romansh
#> 1027         rm-puter                                      Putèr
#> 1028         rm-rumgr                         Rumantsch Grischun
#> 1029      rm-surmiran                                   Surmiran
#> 1030       rm-sursilv                                  Sursilvan
#> 1031       rm-sutsilv                                  Sutsilvan
#> 1032      rm-vallader                                   Vallader
#> 1033              rmc                          Carpathian Romani
#> 1034              rmf                               Finnish Kalo
#> 1035              rmg                        Traveller Norwegian
#> 1036              rml                              Baltic Romani
#> 1037         rml-cyrl            Baltic Romani (Cyrillic script)
#> 1038              rmn                              Balkan Romani
#> 1039              rmo                               Sinte Romani
#> 1040              rmw                               Welsh-Romani
#> 1041              rmy                                Vlax Romani
#> 1042               rn                                      Rundi
#> 1043               ro                                   Romanian
#> 1044            ro-md                                  Moldavian
#> 1045              roa                          Romance languages
#> 1046          roa-rup                                  Aromanian
#> 1047         roa-tara                                  Tarantino
#> 1048              rof                                      Rombo
#> 1049              rom                                     Romany
#> 1050              rsk                            Pannonian Rusyn
#> 1051              rtm                                    Rotuman
#> 1052               ru                                    Russian
#> 1053      ru-petr1708              Russian (Petrine orthography)
#> 1054              rue                                      Rusyn
#> 1055              rug                                    Roviana
#> 1056              ruo                             Istro Romanian
#> 1057              rup                                  Aromanian
#> 1058              ruq                           Megleno-Romanian
#> 1059         ruq-cyrl         Megleno-Romanian (Cyrillic script)
#> 1060         ruq-latn            Megleno-Romanian (Latin script)
#> 1061              rut                                      Rutul
#> 1062               rw                                Kinyarwanda
#> 1063              rwk                                        Rwa
#> 1064              rwr                            Marwari (India)
#> 1065              rys                                    Yaeyama
#> 1066         rys-hira                  Yaeyama (Hiragana script)
#> 1067              ryu                                   Okinawan
#> 1068         ryu-hira                 Okinawan (Hiragana script)
#> 1069               sa                                   Sanskrit
#> 1070          sa-sidd                  Sanskrit (Siddham script)
#> 1071              sad                                    Sandawe
#> 1072              sah                                      Yakut
#> 1073              sai        South American indigenous languages
#> 1074              sal                         Salishan languages
#> 1075              sam                          Samaritan Aramaic
#> 1076              saq                                    Samburu
#> 1077              sas                                      Sasak
#> 1078              sat                                    Santali
#> 1079         sat-beng                   Santali (Bengali script)
#> 1080         sat-latn                     Santali (Latin script)
#> 1081         sat-orya                     Santali (Oriya script)
#> 1082              saz                                 Saurashtra
#> 1083              sba                                    Ngambay
#> 1084              sbp                                      Sangu
#> 1085               sc                                  Sardinian
#> 1086              scl                                      Shina
#> 1087              scn                                   Sicilian
#> 1088              sco                                      Scots
#> 1089               sd                                     Sindhi
#> 1090          sd-deva                 Sindhi (Devanagari script)
#> 1091          sd-gujr                   Sindhi (Gujarati script)
#> 1092          sd-khoj                     Sindhi (Khojki script)
#> 1093          sd-sind                  Sindhi (Khudawadi script)
#> 1094              sdc                        Sassarese Sardinian
#> 1095              sdh                           Southern Kurdish
#> 1096         sdh-arab           Southern Kurdish (Arabic script)
#> 1097         sdh-latn            Southern Kurdish (Latin script)
#> 1098              sdo                               Bukar–Sadong
#> 1099               se                              Northern Sami
#> 1100            se-fi                    Northern Sami (Finland)
#> 1101            se-no                     Northern Sami (Norway)
#> 1102            se-se                     Northern Sami (Sweden)
#> 1103              sea                                      Semai
#> 1104              see                                     Seneca
#> 1105              seh                                       Sena
#> 1106              sei                                       Seri
#> 1107              sel                                     Selkup
#> 1108              sem                          Semitic languages
#> 1109              ser                                    Serrano
#> 1110              ses                            Koyraboro Senni
#> 1111               sg                                      Sango
#> 1112              sga                                  Old Irish
#> 1113              sgh                                    Shughni
#> 1114         sgh-arab                    Shughni (Arabic script)
#> 1115         sgh-cyrl                  Shughni (Cyrillic script)
#> 1116         sgh-latn                     Shughni (Latin script)
#> 1117              sgn                             sign languages
#> 1118              sgs                                 Samogitian
#> 1119         sgy-arab                  Sanglechi (Arabic script)
#> 1120         sgy-latn                   Sanglechi (Latin script)
#> 1121               sh                             Serbo-Croatian
#> 1122          sh-cyrl           Serbo-Croatian (Cyrillic script)
#> 1123          sh-latn              Serbo-Croatian (Latin script)
#> 1124              shd                               Kundal Shahi
#> 1125              shi                                  Tachelhit
#> 1126         shi-latn                   Tachelhit (Latin script)
#> 1127         shi-tfng                Tachelhit (Tifinagh script)
#> 1128              shn                                       Shan
#> 1129              shu                             Chadian Arabic
#> 1130              shy                                    Shawiya
#> 1131         shy-arab                    Shawiya (Arabic script)
#> 1132         shy-latn                     Shawiya (Latin script)
#> 1133         shy-tfng                  Shawiya (Tifinagh script)
#> 1134               si                                    Sinhala
#> 1135              sia                                Akkala Sami
#> 1136              sid                                     Sidamo
#> 1137           simple                             Simple English
#> 1138              sio                           Siouan languages
#> 1139              sit                     Sino-Tibetan languages
#> 1140              sjd                                Kildin Sami
#> 1141              sje                                  Pite Sami
#> 1142              sjk                                  Kemi Sami
#> 1143              sjn                                   Sindarin
#> 1144              sjo                                       Xibe
#> 1145              sjt                                   Ter Sami
#> 1146              sju                                   Ume Sami
#> 1147               sk                                     Slovak
#> 1148              skr                                    Saraiki
#> 1149         skr-arab                    Saraiki (Arabic script)
#> 1150               sl                                  Slovenian
#> 1151              sla                           Slavic languages
#> 1152              slh                       Southern Lushootseed
#> 1153              sli                             Lower Silesian
#> 1154              slr                                      Salar
#> 1155              sly                                    Selayar
#> 1156               sm                                     Samoan
#> 1157              sma                              Southern Sami
#> 1158              smi                             Sámi languages
#> 1159              smj                                  Lule Sami
#> 1160              smn                                 Inari Sami
#> 1161              sms                                 Skolt Sami
#> 1162               sn                                      Shona
#> 1163              sne                                      Jagoi
#> 1164              snk                                    Soninke
#> 1165               so                                     Somali
#> 1166              sog                                    Sogdien
#> 1167              son                          Songhay languages
#> 1168              spv                                 Sambalpuri
#> 1169               sq                                   Albanian
#> 1170               sr                                    Serbian
#> 1171          sr-cyrl                  Serbian (Cyrillic script)
#> 1172            sr-ec                  Serbian (Cyrillic script)
#> 1173            sr-el                     Serbian (Latin script)
#> 1174          sr-latn                     Serbian (Latin script)
#> 1175            sr-me                                Montenegrin
#> 1176         srh-arab                   Sarikoli (Arabic script)
#> 1177         srh-cyrl                 Sarikoli (Cyrillic script)
#> 1178         srh-latn                    Sarikoli (Latin script)
#> 1179              srk                                   Serudung
#> 1180              srn                               Sranan Tongo
#> 1181              sro                      Campidanese Sardinian
#> 1182              srq                                    Sirionó
#> 1183              srr                                      Serer
#> 1184               ss                                      Swati
#> 1185              ssa                     Nilo-Saharan languages
#> 1186              ssb                              Southern Sama
#> 1187              ssf                                       Thao
#> 1188              ssy                                       Saho
#> 1189               st                             Southern Sotho
#> 1190              sth                                     Shelta
#> 1191              stq                          Saterland Frisian
#> 1192              str                             Straits Salish
#> 1193              sty                             Siberian Tatar
#> 1194               su                                  Sundanese
#> 1195              suk                                     Sukuma
#> 1196              sus                                       Susu
#> 1197              sux                                   Sumerian
#> 1198         sux-latn                    Sumerian (Latin script)
#> 1199         sux-xsux                Sumerian (Cuneiform script)
#> 1200              suz                                     Sunwar
#> 1201               sv                                    Swedish
#> 1202              sva                                       Svan
#> 1203               sw                                    Swahili
#> 1204            sw-cd                              Congo Swahili
#> 1205              swb                                   Comorian
#> 1206              sxr                                     Saaroa
#> 1207              sxu                                Upper Saxon
#> 1208              syc                           Classical Syriac
#> 1209              syl                                    Sylheti
#> 1210         syl-beng                   Sylheti (Bengali script)
#> 1211         syl-sylo             Sylheti (Sylheti Nagri script)
#> 1212              syr                                     Syriac
#> 1213              szl                                   Silesian
#> 1214              szy                                   Sakizaya
#> 1215               ta                                      Tamil
#> 1216              tai                              Tai languages
#> 1217              tao                                       Yami
#> 1218              tay                                     Atayal
#> 1219              tbl                                      Tboli
#> 1220              tce                          Southern Tutchone
#> 1221              tcy                                       Tulu
#> 1222              tdd                                   Tai Nuea
#> 1223               te                                     Telugu
#> 1224              tem                                      Timne
#> 1225              teo                                       Teso
#> 1226              ter                                     Tereno
#> 1227              tet                                      Tetum
#> 1228               tg                                      Tajik
#> 1229          tg-cyrl                    Tajik (Cyrillic script)
#> 1230          tg-latn                       Tajik (Latin script)
#> 1231              tgx                                     Tagish
#> 1232               th                                       Thai
#> 1233              thq                              Kochila Tharu
#> 1234              thr                                 Rana Tharu
#> 1235              tht                                    Tahltan
#> 1236               ti                                   Tigrinya
#> 1237              tig                                      Tigre
#> 1238              tih                                    Timugon
#> 1239              tiv                                        Tiv
#> 1240              tji                             Northern Tujia
#> 1241               tk                                    Turkmen
#> 1242              tkl                                  Tokelauan
#> 1243              tkr                                    Tsakhur
#> 1244               tl                                    Tagalog
#> 1245              tlb                                     Tobelo
#> 1246              tlh                                    Klingon
#> 1247         tlh-latn                     Klingon (Latin script)
#> 1248         tlh-piqd                   Klingon (Klingon script)
#> 1249              tli                                    Tlingit
#> 1250              tly                                     Talysh
#> 1251         tly-cyrl                   Talysh (Cyrillic script)
#> 1252              tmh                                   Tamashek
#> 1253              tmr                  Jewish Babylonian Aramaic
#> 1254               tn                                     Tswana
#> 1255              tnq                                      Taíno
#> 1256               to                                     Tongan
#> 1257              tog                                Nyasa Tonga
#> 1258              toi                            Tonga (Botatwe)
#> 1259              tok                                  Toki Pona
#> 1260              tpi                                  Tok Pisin
#> 1261               tr                                    Turkish
#> 1262              trp                                   Kokborok
#> 1263              tru                                     Turoyo
#> 1264              trv                                     Taroko
#> 1265              trw                                    Torwali
#> 1266               ts                                     Tsonga
#> 1267              tsd                                  Tsakonian
#> 1268              tsg                                     Tausug
#> 1269              tsi                                  Tsimshian
#> 1270              tsu                                       Tsou
#> 1271               tt                                      Tatar
#> 1272          tt-cyrl                    Tatar (Cyrillic script)
#> 1273          tt-latn                       Tatar (Latin script)
#> 1274              ttj                                      Tooro
#> 1275              ttm                          Northern Tutchone
#> 1276              ttt                                 Muslim Tat
#> 1277              tui                                     Tupuri
#> 1278              tum                                    Tumbuka
#> 1279              tup                           Tupian languages
#> 1280              tut                           Altaic languages
#> 1281              tvl                                     Tuvalu
#> 1282              tvu                                      Tunen
#> 1283               tw                                        Twi
#> 1284              twd                                    Tweants
#> 1285              twq                                    Tasawaq
#> 1286              txa                                  Tombonuwo
#> 1287              txg                                     Tangut
#> 1288         txo-beng                      Toto (Bengali script)
#> 1289         txo-toto                         Toto (Toto script)
#> 1290              txx                                     Tatana
#> 1291               ty                                   Tahitian
#> 1292              tyv                                   Tuvinian
#> 1293              tzl                                   Talossan
#> 1294              tzm                    Central Atlas Tamazight
#> 1295              udm                                     Udmurt
#> 1296               ug                                     Uyghur
#> 1297          ug-arab                     Uyghur (Arabic script)
#> 1298          ug-cyrl                   Uyghur (Cyrillic script)
#> 1299          ug-latn                      Uyghur (Latin script)
#> 1300              uga                                   Ugaritic
#> 1301               uk                                  Ukrainian
#> 1302              ulc                                       Ulch
#> 1303              uln                               Unserdeutsch
#> 1304              umb                                    Umbundu
#> 1305              umu                                     Munsee
#> 1306              und                      undetermined language
#> 1307              unr                                    Mundari
#> 1308         unr-deva                Mundari (Devanagari script)
#> 1309         unr-nagm               Mundari (Nag Mundari script)
#> 1310               ur                                       Urdu
#> 1311              urk                                Urak Lawoiʼ
#> 1312              ush                                     Ushoji
#> 1313              uun                                      Pazeh
#> 1314               uz                                      Uzbek
#> 1315          uz-cyrl                    Uzbek (Cyrillic script)
#> 1316          uz-latn                       Uzbek (Latin script)
#> 1317              vai                                        Vai
#> 1318               ve                                      Venda
#> 1319              vec                                   Venetian
#> 1320              vep                                       Veps
#> 1321               vi                                 Vietnamese
#> 1322          vi-hani                    Vietnamese (Han script)
#> 1323              vls                               West Flemish
#> 1324              vmf                            Main-Franconian
#> 1325              vmw                                    Makhuwa
#> 1326               vo                                    Volapük
#> 1327              vot                                      Votic
#> 1328              vro                                       Võro
#> 1329              vun                                      Vunjo
#> 1330              vut                                       Vute
#> 1331               wa                                    Walloon
#> 1332              wae                                     Walser
#> 1333              wak                         Wakashan languages
#> 1334              wal                                   Wolaytta
#> 1335              war                                      Waray
#> 1336              was                                      Washo
#> 1337         wbl-arab                      Wakhi (Arabic script)
#> 1338      wbl-arab-af         Wakhi (Arabic script, Afghanistan)
#> 1339      wbl-arab-cn               Wakhi (Arabic script, China)
#> 1340      wbl-arab-pk            Wakhi (Arabic script, Pakistan)
#> 1341         wbl-cyrl                    Wakhi (Cyrillic script)
#> 1342         wbl-latn                       Wakhi (Latin script)
#> 1343              wbp                                   Warlpiri
#> 1344              wen                          Sorbian languages
#> 1345              wes                          Pidgin (Cameroon)
#> 1346              wlm                               Middle Welsh
#> 1347              wls                                  Wallisian
#> 1348              wlx                                       Wali
#> 1349               wo                                      Wolof
#> 1350              wsg                             Adilabad Gondi
#> 1351              wsv                        Wotapuri-Katarqalai
#> 1352              wuu                                         Wu
#> 1353         wuu-hans                 Wu (Simplified Han script)
#> 1354         wuu-hant                Wu (Traditional Han script)
#> 1355              wya                                    Wyandot
#> 1356              wyi                                 Woiwurrung
#> 1357              xal                                     Kalmyk
#> 1358              xbm                              Middle Breton
#> 1359               xh                                      Xhosa
#> 1360              xmf                                 Mingrelian
#> 1361              xmm                               Manado Malay
#> 1362              xnb                                 Kanakanavu
#> 1363              xno                               Anglo-Norman
#> 1364              xnr                                     Kangri
#> 1365         xnr-deva                 Kangri (Devanagari script)
#> 1366         xnr-takr                      Kangri (Takri script)
#> 1367              xog                                       Soga
#> 1368              xon                                   Konkomba
#> 1369              xpu                                      Punic
#> 1370              xsu                                     Sanumá
#> 1371              xsy                                   Saisiyat
#> 1372         yah-cyrl               Yazghulami (Cyrillic script)
#> 1373         yah-latn                  Yazghulami (Latin script)
#> 1374         yai-cyrl                 Yaghnobi (Cyrillic script)
#> 1375         yai-latn                    Yaghnobi (Latin script)
#> 1376              yao                                        Yao
#> 1377              yap                                     Yapese
#> 1378              yas                                     Nugunu
#> 1379              yat                                    Yambeta
#> 1380              yav                                    Yangben
#> 1381              ybb                                      Yemba
#> 1382              ydd                            Eastern Yiddish
#> 1383              ydg                                     Yidgha
#> 1384              yec                                    Yeniche
#> 1385               yi                                    Yiddish
#> 1386              ykg                            Tundra Yukaghir
#> 1387               yo                                     Yoruba
#> 1388              yoi                                   Yonaguni
#> 1389         yoi-hira                 Yonaguni (Hiragana script)
#> 1390              yox                                      Yoron
#> 1391         yox-hira                    Yoron (Hiragana script)
#> 1392              ypk                            Yupik languages
#> 1393              yrk                                     Nenets
#> 1394              yrl                                  Nheengatu
#> 1395              yua                               Yucatec Maya
#> 1396              yue                                  Cantonese
#> 1397         yue-hans          Cantonese (Simplified Han script)
#> 1398         yue-hant         Cantonese (Traditional Han script)
#> 1399               za                                     Zhuang
#> 1400              zai                            Isthmus Zapotec
#> 1401              zap                                    Zapotec
#> 1402              zbl                                Blissymbols
#> 1403              zea                                  Zeelandic
#> 1404              zen                                     Zenaga
#> 1405              zgh                Standard Moroccan Tamazight
#> 1406         zgh-latn Standard Moroccan Tamazight (Latin script)
#> 1407               zh                                    Chinese
#> 1408     zh-classical                           Literary Chinese
#> 1409            zh-cn                            Chinese (China)
#> 1410          zh-hans                         Simplified Chinese
#> 1411          zh-hant                        Traditional Chinese
#> 1412            zh-hk                        Chinese (Hong Kong)
#> 1413       zh-min-nan                                     Minnan
#> 1414            zh-mo                            Chinese (Macau)
#> 1415            zh-my                         Chinese (Malaysia)
#> 1416            zh-sg                        Chinese (Singapore)
#> 1417            zh-tw                           Chinese (Taiwan)
#> 1418           zh-yue                                  Cantonese
#> 1419              zmi                      Negeri Sembilan Malay
#> 1420              znd                            Zande languages
#> 1421              zpu                            Yalálag Zapotec
#> 1422               zu                                       Zulu
#> 1423              zun                                       Zuni
#> 1424              zxx                      no linguistic content
#> 1425              zza                                       Zaza
#>                              autonym
#> 1                           Qafár af
#> 2                          Arbërisht
#> 3                             аԥсшәа
#> 4                                   
#> 5                                   
#> 6                                   
#> 7                              Abron
#> 8                       bahasa ambon
#> 9                               Acèh
#> 10                  Kwéyòl Sent Lisi
#> 11                                  
#> 12                             عراقي
#> 13                                  
#> 14                                  
#> 15                          адыгабзэ
#> 16                          адыгабзэ
#> 17                                  
#> 18                                  
#> 19                     تونسي / Tûnsî
#> 20                             تونسي
#> 21                             Tûnsî
#> 22                                  
#> 23                                  
#> 24                                  
#> 25                         Afrikaans
#> 26                                  
#> 27                                  
#> 28                                  
#> 29                                  
#> 30                                  
#> 31          Aanteegan an' Baabyuudan
#> 32                                  
#> 33                                  
#> 34                                  
#> 35                                  
#> 36                                  
#> 37                                  
#> 38                              Akan
#> 39                                  
#> 40                                  
#> 41                                  
#> 42                                  
#> 43                                  
#> 44                                  
#> 45                                  
#> 46                                  
#> 47                                  
#> 48                              Gegë
#> 49                                  
#> 50                       Alemannisch
#> 51                         алтай тил
#> 52                                  
#> 53                              አማርኛ
#> 54                           Pangcah
#> 55                                  
#> 56                          aragonés
#> 57                                  
#> 58                           Ænglisc
#> 59                             Obolo
#> 60                             अंगिका
#> 61                                  
#> 62                              شامي
#> 63                                  
#> 64                                  
#> 65                                  
#> 66                           العربية
#> 67                                  
#> 68                             ܐܪܡܝܐ
#> 69                                  
#> 70                        mapudungun
#> 71                                  
#> 72                                  
#> 73                          جازايرية
#> 74                                  
#> 75                                  
#> 76                                  
#> 77                           الدارجة
#> 78                                  
#> 79                                  
#> 80                              مصرى
#> 81                            অসমীয়া
#> 82                                  
#> 83            American sign language
#> 84                         asturianu
#> 85                                  
#> 86                         Atikamekw
#> 87                                  
#> 88                                  
#> 89                              авар
#> 90                            Kotava
#> 91                              अवधी
#> 92                                  
#> 93                                  
#> 94                         Aymar aru
#> 95                                  
#> 96                      azərbaycanca
#> 97                                  
#> 98                                  
#> 99                                  
#> 100                           تۆرکجه
#> 101                                 
#> 102                        башҡортса
#> 103                                 
#> 104                                 
#> 105                                 
#> 106                                 
#> 107                                 
#> 108                        Basa Bali
#> 109                             ᬩᬲᬩᬮᬶ
#> 110                         Boarisch
#> 111                                 
#> 112                                 
#> 113                       žemaitėška
#> 114                                 
#> 115                       Batak Toba
#> 116                                 
#> 117                       Batak Toba
#> 118                                 
#> 119                     جهلسری بلوچی
#> 120                            wawle
#> 121                    Bikol Central
#> 122                       Bajau Sama
#> 123                       беларуская
#> 124         беларуская (тарашкевіца)
#> 125         беларуская (тарашкевіца)
#> 126                                 
#> 127                                 
#> 128                                 
#> 129                           Betawi
#> 130                                 
#> 131                                 
#> 132                                 
#> 133                                 
#> 134                                 
#> 135                                 
#> 136                                 
#> 137                                 
#> 138                                 
#> 139                                 
#> 140                                 
#> 141                        български
#> 142                         हरियाणवी
#> 143                                 
#> 144                                 
#> 145                  روچ کپتین بلوچی
#> 146                                 
#> 147                                 
#> 148                                 
#> 149                                 
#> 150                           भोजपुरी
#> 151                                 
#> 152                                 
#> 153                                 
#> 154                                 
#> 155                           भोजपुरी
#> 156                          Bislama
#> 157                                 
#> 158                                 
#> 159                           Banjar
#> 160                                 
#> 161                                 
#> 162                                 
#> 163                                 
#> 164                                 
#> 165                                 
#> 166                       ပအိုဝ်ႏဘာႏသာႏ
#> 167                                 
#> 168                                 
#> 169                       bamanankan
#> 170                            বাংলা
#> 171                                 
#> 172                                 
#> 173                                 
#> 174                                 
#> 175                            བོད་ཡིག
#> 176                        bòo pìkkà
#> 177                                 
#> 178                 বিষ্ণুপ্রিয়া মণিপুরী
#> 179                          بختیاری
#> 180                                 
#> 181                        brezhoneg
#> 182                                 
#> 183                           Bráhuí
#> 184                                 
#> 185                                 
#> 186                         bosanski
#> 187                                 
#> 188                                 
#> 189                                 
#> 190                                 
#> 191                                 
#> 192                                 
#> 193                 Batak Mandailing
#> 194                   Iriga Bicolano
#> 195                                 
#> 196                                 
#> 197                                 
#> 198                                 
#> 199                         Basa Ugi
#> 200                            ᨅᨔ ᨕᨘᨁᨗ
#> 201                                 
#> 202                                 
#> 203                                 
#> 204                           буряад
#> 205                                 
#> 206                                 
#> 207                                 
#> 208                                 
#> 209                           català
#> 210                                 
#> 211                                 
#> 212                                 
#> 213                                 
#> 214                                 
#> 215                                 
#> 216                                 
#> 217           Chavacano de Zamboanga
#> 218           Chavacano de Zamboanga
#> 219                                 
#> 220                             𑄌𑄋𑄴𑄟𑄳𑄦
#> 221                                 
#> 222           閩東語 / Mìng-dĕ̤ng-ngṳ̄
#> 223                                 
#> 224               閩東語（傳統漢字）
#> 225       Mìng-dĕ̤ng-ngṳ̄ (Bàng-uâ-cê)
#> 226                                 
#> 227                          нохчийн
#> 228                          Cebuano
#> 229                                 
#> 230                                 
#> 231                          Chamoru
#> 232                                 
#> 233                                 
#> 234                                 
#> 235                                 
#> 236                      chinuk wawa
#> 237                    Chahta anumpa
#> 238                                 
#> 239                              ᏣᎳᎩ
#> 240                  Tsetsêhestâhese
#> 241                                 
#> 242                                 
#> 243                                 
#> 244                                 
#> 245                                 
#> 246                                 
#> 247                                 
#> 248                                 
#> 249                                 
#> 250                                 
#> 251                                 
#> 252                                 
#> 253                                 
#> 254                            کوردی
#> 255                                 
#> 256                                 
#> 257                                 
#> 258                                 
#> 259                                 
#> 260                                 
#> 261                                 
#> 262                                 
#> 263                                 
#> 264                                 
#> 265                                 
#> 266                                 
#> 267                                 
#> 268                            corsu
#> 269                                 
#> 270                     ϯⲙⲉⲧⲣⲉⲙⲛ̀ⲭⲏⲙⲓ
#> 271                                 
#> 272                                 
#> 273                                 
#> 274                         Capiceño
#> 275              莆仙語 / Pó-sing-gṳ̂
#> 276                   莆仙语（简体）
#> 277                   莆仙語（繁體）
#> 278           Pó-sing-gṳ̂ (Báⁿ-uā-ci̍)
#> 279            Nēhiyawēwin / ᓀᐦᐃᔭᐍᐏᐣ
#> 280                                 
#> 281                                 
#> 282                                 
#> 283                                 
#> 284                     qırımtatarca
#> 285          къырымтатарджа (Кирилл)
#> 286             qırımtatarca (Latin)
#> 287                          tatarşa
#> 288                                 
#> 289                                 
#> 290                                 
#> 291                                 
#> 292                                 
#> 293                                 
#> 294                                 
#> 295                          čeština
#> 296                       kaszëbsczi
#> 297                                 
#> 298                                 
#> 299          словѣньскъ / ⰔⰎⰑⰂⰡⰐⰠⰔⰍⰟ
#> 300                                 
#> 301                          чӑвашла
#> 302                          Cymraeg
#> 303                            dansk
#> 304                         dagbanli
#> 305                                 
#> 306                                 
#> 307                                 
#> 308                                 
#> 309                                 
#> 310                                 
#> 311                          Deutsch
#> 312                                 
#> 313         Österreichisches Deutsch
#> 314            Schweizer Hochdeutsch
#> 315               Deutsch (Sie-Form)
#> 316                                 
#> 317                                 
#> 318                          Dagaare
#> 319                                 
#> 320                         Thuɔŋjäŋ
#> 321                           Zazaki
#> 322                                 
#> 323                                 
#> 324                      долган тыла
#> 325                                 
#> 326                                 
#> 327                                 
#> 328                                 
#> 329                                 
#> 330                                 
#> 331                                 
#> 332                                 
#> 333                                 
#> 334                                 
#> 335                                 
#> 336                     dolnoserbski
#> 337                                 
#> 338                                 
#> 339                     Kadazandusun
#> 340                                 
#> 341                            डोटेली
#> 342                            Duálá
#> 343                                 
#> 344                                 
#> 345                            ދިވެހިބަސް
#> 346                                 
#> 347                                 
#> 348                             ཇོང་ཁ
#> 349                                 
#> 350                                 
#> 351                           eʋegbe
#> 352                             Efịk
#> 353               emiliàn e rumagnòl
#> 354                                 
#> 355                                 
#> 356                                 
#> 357                         Ελληνικά
#> 358                                 
#> 359                                 
#> 360                                 
#> 361               emiliàn e rumagnòl
#> 362                          English
#> 363                                 
#> 364                 Canadian English
#> 365                  British English
#> 366                                 
#> 367                                 
#> 368                                 
#> 369                   Simple English
#> 370                                 
#> 371                                 
#> 372                                 
#> 373                        Esperanto
#> 374                                 
#> 375                                 
#> 376                          español
#> 377        español de América Latina
#> 378                                 
#> 379                 español (formal)
#> 380                                 
#> 381                                 
#> 382                                 
#> 383                                 
#> 384                            eesti
#> 385                                 
#> 386                                 
#> 387                                 
#> 388                          euskara
#> 389                                 
#> 390                        estremeñu
#> 391                                 
#> 392                            فارسی
#> 393                                 
#> 394                                 
#> 395                                 
#> 396                          mfantse
#> 397                                 
#> 398                                 
#> 399                         Fulfulde
#> 400                            suomi
#> 401                                 
#> 402                        meänkieli
#> 403                                 
#> 404                             võro
#> 405                 Na Vosa Vakaviti
#> 406                                 
#> 407                                 
#> 408                         føroyskt
#> 409                           fɔ̀ngbè
#> 410                                 
#> 411                         français
#> 412                                 
#> 413                                 
#> 414                                 
#> 415                  français cadien
#> 416                                 
#> 417                                 
#> 418                                 
#> 419                          arpetan
#> 420                       Nordfriisk
#> 421                       Oostfräisk
#> 422                                 
#> 423                                 
#> 424                                 
#> 425                           furlan
#> 426                   poor’íŋ belé’ŋ
#> 427                            Frysk
#> 428                          Gaeilge
#> 429                               Ga
#> 430                           Gagauz
#> 431                                 
#> 432                             贛語
#> 433                     赣语（简体）
#> 434                     贛語（繁體）
#> 435                                 
#> 436                                 
#> 437                                 
#> 438                                 
#> 439                                 
#> 440                                 
#> 441                                 
#> 442                                 
#> 443                  kréyòl Gwadloup
#> 444                 kriyòl gwiyannen
#> 445                         Gàidhlig
#> 446                                 
#> 447                                 
#> 448                                 
#> 449                                 
#> 450                                 
#> 451                                 
#> 452                           galego
#> 453                             на̄ни
#> 454                                 
#> 455                            گیلکی
#> 456                                 
#> 457                                 
#> 458                                 
#> 459                          Avañe'ẽ
#> 460                                 
#> 461                                 
#> 462     गोंयची कोंकणी / Gõychi Konknni
#> 463                      गोंयची कोंकणी
#> 464                   Gõychi Konknni
#> 465                                 
#> 466                 Bahasa Hulontalo
#> 467                           𐌲𐌿𐍄𐌹𐍃𐌺
#> 468                  Ghanaian Pidgin
#> 469                                 
#> 470                  Ἀρχαία ἑλληνικὴ
#> 471                                 
#> 472                      Alemannisch
#> 473                                 
#> 474                           ગુજરાતી
#> 475                       wayuunaiki
#> 476                                 
#> 477                         farefare
#> 478                           gungbe
#> 479                                 
#> 480                            Gaelg
#> 481                                 
#> 482                                 
#> 483                            Hausa
#> 484                                 
#> 485                                 
#> 486                                 
#> 487                                 
#> 488                                 
#> 489              客家語 / Hak-kâ-ngî
#> 490                   客家语（简体）
#> 491                   客家語（繁體）
#> 492          Hak-kâ-ngî (Pha̍k-fa-sṳ)
#> 493                                 
#> 494                          Hawaiʻi
#> 495                                 
#> 496                                 
#> 497                                 
#> 498                            עברית
#> 499                                 
#> 500                            हिन्दी
#> 501                                 
#> 502                                 
#> 503                       Fiji Hindi
#> 504                                 
#> 505                       Fiji Hindi
#> 506                          Ilonggo
#> 507                                 
#> 508                                 
#> 509                                 
#> 510                                 
#> 511                          kihunde
#> 512                                 
#> 513                                 
#> 514                                 
#> 515                            ہندکو
#> 516                        Hiri Motu
#> 517                                 
#> 518                               Ho
#> 519                         hrvatski
#> 520                          Hunsrik
#> 521                    hornjoserbsce
#> 522                             湘語
#> 523                                 
#> 524                                 
#> 525                   Kreyòl ayisyen
#> 526                                 
#> 527                           magyar
#> 528                  magyar (formal)
#> 529                                 
#> 530                                 
#> 531                          հայերեն
#> 532                   Արեւմտահայերէն
#> 533                       Otsiherero
#> 534                      interlingua
#> 535                        Jaku Iban
#> 536                           ibibio
#> 537                 Bahasa Indonesia
#> 538                      Interlingue
#> 539                                 
#> 540                             Igbo
#> 541                                 
#> 542                            Igala
#> 543                             ꆇꉙ
#> 544                                 
#> 545                        Iñupiatun
#> 546                           ᐃᓄᒃᑎᑐᑦ
#> 547                        inuktitut
#> 548                                 
#> 549                          Ilokano
#> 550                                 
#> 551                                 
#> 552                         гӀалгӀай
#> 553                              Ido
#> 554                                 
#> 555                                 
#> 556                         íslenska
#> 557                                 
#> 558                                 
#> 559                                 
#> 560                                 
#> 561                                 
#> 562                                 
#> 563                  medžuslovjansky
#> 564                  меджусловјанскы
#> 565                  medžuslovjansky
#> 566                         italiano
#> 567               ᐃᓄᒃᑎᑐᑦ / inuktitut
#> 568                                 
#> 569                                 
#> 570                           日本語
#> 571                                 
#> 572                                 
#> 573                                 
#> 574                                 
#> 575                                 
#> 576                                 
#> 577                           Patois
#> 578                                 
#> 579                      la .lojban.
#> 580                                 
#> 581                                 
#> 582                                 
#> 583                                 
#> 584                                 
#> 585                                 
#> 586                                 
#> 587                                 
#> 588                             jysk
#> 589                             Jawa
#> 590                               ꦗꦮ
#> 591                          ქართული
#> 592                    Qaraqalpaqsha
#> 593                        Taqbaylit
#> 594                                 
#> 595                                 
#> 596                      Karai-karai
#> 597                              Jju
#> 598                                 
#> 599                                 
#> 600                                 
#> 601                         адыгэбзэ
#> 602                         адыгэбзэ
#> 603                                 
#> 604                                 
#> 605                           Kabɩyɛ
#> 606                             Tyap
#> 607                                 
#> 608                                 
#> 609                     kabuverdianu
#> 610                                 
#> 611                                 
#> 612                                 
#> 613                                 
#> 614                                 
#> 615                            Kongo
#> 616                         Kumoring
#> 617                                 
#> 618                                 
#> 619                                 
#> 620                                 
#> 621                                 
#> 622                                 
#> 623                                 
#> 624                            کھوار
#> 625                           Gĩkũyũ
#> 626                                 
#> 627                        Kırmancki
#> 628                         Kwanyama
#> 629                            хакас
#> 630                              ဖၠုံလိက်
#> 631                          қазақша
#> 632                  قازاقشا (تٴوتە)
#> 633                  قازاقشا (جۇنگو)
#> 634                  қазақша (кирил)
#> 635              қазақша (Қазақстан)
#> 636                  qazaqşa (latın)
#> 637                qazaqşa (Türkïya)
#> 638                                 
#> 639                      kalaallisut
#> 640                                 
#> 641                                 
#> 642                                 
#> 643                                 
#> 644                                 
#> 645                         ភាសាខ្មែរ
#> 646                                 
#> 647                                 
#> 648                                 
#> 649                                 
#> 650                                 
#> 651                             ಕನ್ನಡ
#> 652                     Yerwa Kanuri
#> 653                                 
#> 654                                 
#> 655                                 
#> 656                           한국어
#> 657                                 
#> 658                                 
#> 659                                 
#> 660                           조선말
#> 661                       перем коми
#> 662                                 
#> 663                                 
#> 664                                 
#> 665                                 
#> 666                                 
#> 667                                 
#> 668                                 
#> 669                           kanuri
#> 670                 къарачай-малкъар
#> 671                             Krio
#> 672                        Kinaray-a
#> 673                           karjal
#> 674                                 
#> 675                                 
#> 676                             کٲشُر
#> 677                             کٲشُر
#> 678                             कॉशुर
#> 679                                 
#> 680                                 
#> 681                       Ripoarisch
#> 682                               စှီၤ
#> 683                                 
#> 684                            kurdî
#> 685                   کوردی (عەرەبی)
#> 686                   kurdî (latînî)
#> 687                          къумукъ
#> 688                           Kʋsaal
#> 689                                 
#> 690                             коми
#> 691                                 
#> 692                         kernowek
#> 693                                 
#> 694                                 
#> 695                                 
#> 696                                 
#> 697                                 
#> 698                         кыргызча
#> 699                                 
#> 700                                 
#> 701                           Latina
#> 702                           Ladino
#> 703                                 
#> 704                                 
#> 705                                 
#> 706                                 
#> 707                                 
#> 708                                 
#> 709                   Lëtzebuergesch
#> 710                            лакку
#> 711                                 
#> 712                                 
#> 713                                 
#> 714                            лезги
#> 715               Lingua Franca Nova
#> 716                          Luganda
#> 717                         Limburgs
#> 718                                 
#> 719                                 
#> 720                           Ligure
#> 721                                 
#> 722                                 
#> 723                         Līvõ kēļ
#> 724                      Lampung Api
#> 725                             لەکی
#> 726                      Lakȟótiyapi
#> 727                            Ladin
#> 728                                 
#> 729                          lombard
#> 730                          lingála
#> 731                                 
#> 732                              ລາວ
#> 733                                 
#> 734                                 
#> 735                           Silozi
#> 736                      لۊری شومالی
#> 737                                 
#> 738                         lietuvių
#> 739                          latgaļu
#> 740                                 
#> 741                           ciluba
#> 742                                 
#> 743                                 
#> 744                                 
#> 745                                 
#> 746                       Mizo ţawng
#> 747                                 
#> 748                                 
#> 749                      لئری دوٙمینی
#> 750                         latviešu
#> 751                             文言
#> 752                           Lazuri
#> 753                          Madhurâ
#> 754                                 
#> 755                             मगही
#> 756                            मैथिली
#> 757                                 
#> 758                                 
#> 759                                 
#> 760                                 
#> 761                  Basa Banyumasan
#> 762                                 
#> 763                                 
#> 764                                 
#> 765                                 
#> 766                                 
#> 767                          мокшень
#> 768                                 
#> 769                                 
#> 770                                 
#> 771                                 
#> 772                                 
#> 773                                 
#> 774                                 
#> 775                         Malagasy
#> 776                                 
#> 777                                 
#> 778                                 
#> 779                             Ebon
#> 780                                 
#> 781                                 
#> 782                       олык марий
#> 783                            Māori
#> 784                                 
#> 785                                 
#> 786                      Minangkabau
#> 787                                 
#> 788                                 
#> 789                                 
#> 790                                 
#> 791                       македонски
#> 792                                 
#> 793                           മലയാളം
#> 794                           монгол
#> 795                                 
#> 796                                 
#> 797                      manju gisun
#> 798                      manju gisun
#> 799                      ᠮᠠᠨᠵᡠ ᡤᡳᠰᡠᠨ
#> 800                         ꯃꯤꯇꯩ ꯂꯣꯟ
#> 801                                 
#> 802                                 
#> 803                                 
#> 804                                 
#> 805                                 
#> 806                           ဘာသာမန်
#> 807                     молдовеняскэ
#> 808                                 
#> 809                                 
#> 810                            moore
#> 811                            मराठी
#> 812                                 
#> 813                             Mara
#> 814                       кырык мары
#> 815                                 
#> 816                                 
#> 817                    Bahasa Melayu
#> 818                       بهاس ملايو
#> 819                                 
#> 820                            Malti
#> 821                                 
#> 822                   Baso Palembang
#> 823                                 
#> 824                                 
#> 825                          Mvskoke
#> 826                                 
#> 827                                 
#> 828                                 
#> 829                                 
#> 830                         Mirandés
#> 831                                 
#> 832                                 
#> 833                                 
#> 834                                 
#> 835                        မြန်မာဘာသာ
#> 836                                 
#> 837                                 
#> 838                           эрзянь
#> 839                          مازِرونی
#> 840                   Dorerin Naoero
#> 841                          Nāhuatl
#> 842                                 
#> 843              閩南語 / Bân-lâm-gí
#> 844                                 
#> 845                                 
#> 846               閩南語（傳統漢字）
#> 847           Bân-lâm-gí (Pe̍h-ōe-jī)
#> 848              Bân-lâm-gí (Tâi-lô)
#> 849                       Napulitano
#> 850                                 
#> 851                     norsk bokmål
#> 852                                 
#> 853                     Plattdüütsch
#> 854                     Nedersaksies
#> 855                            नेपाली
#> 856                        नेपाल भाषा
#> 857                        Oshiwambo
#> 858                                 
#> 859                          Li Niha
#> 860                                 
#> 861                              కొలామి
#> 862                             Niuē
#> 863                                 
#> 864                       Nederlands
#> 865                                 
#> 866           Nederlands (informeel)
#> 867                                 
#> 868                                 
#> 869                            nawdm
#> 870                    norsk nynorsk
#> 871                                 
#> 872                                 
#> 873                                 
#> 874                            norsk
#> 875                            ᨣᩤᩴᨾᩮᩬᩥᨦ
#> 876                                 
#> 877                          ногайша
#> 878                                 
#> 879                                 
#> 880                           Novial
#> 881                              ߒߞߏ
#> 882              isiNdebele seSewula
#> 883                                 
#> 884                                 
#> 885                        Nouormand
#> 886                                 
#> 887                                 
#> 888                 Sesotho sa Leboa
#> 889                                 
#> 890                                 
#> 891                             Nupe
#> 892                                 
#> 893                      Diné bizaad
#> 894                                 
#> 895                                 
#> 896                        Chi-Chewa
#> 897                                 
#> 898                       runyankore
#> 899                         Orunyoro
#> 900                           Nyunga
#> 901                                 
#> 902                                 
#> 903                          occitan
#> 904                                 
#> 905                                 
#> 906                                 
#> 907                                 
#> 908                      Ojibwemowin
#> 909                                 
#> 910                                 
#> 911                                 
#> 912                                 
#> 913                                 
#> 914                                 
#> 915                                 
#> 916                    livvinkarjala
#> 917                           Oromoo
#> 918                                 
#> 919                                 
#> 920                              ଓଡ଼ିଆ
#> 921                             ирон
#> 922                                 
#> 923                                 
#> 924                                 
#> 925                                 
#> 926                                 
#> 927                                 
#> 928                                 
#> 929                                 
#> 930                                 
#> 931                            ਪੰਜਾਬੀ
#> 932                                 
#> 933                                 
#> 934                       Pangasinan
#> 935                                 
#> 936                                 
#> 937                                 
#> 938                                 
#> 939                      Kapampangan
#> 940                                 
#> 941                       Papiamentu
#> 942               Papiamento (Aruba)
#> 943                                 
#> 944                                 
#> 945                                 
#> 946                           Picard
#> 947                            Naijá
#> 948                          Deitsch
#> 949                     Plautdietsch
#> 950                                 
#> 951                         Pälzisch
#> 952                                 
#> 953                                 
#> 954                                 
#> 955                                 
#> 956                                 
#> 957                                 
#> 958                                 
#> 959                                 
#> 960                                 
#> 961                                 
#> 962                                 
#> 963                             पालि
#> 964                                 
#> 965                 Norfuk / Pitkern
#> 966                                 
#> 967                                 
#> 968                                 
#> 969                                 
#> 970                                 
#> 971                           polski
#> 972                                 
#> 973                                 
#> 974                       Piemontèis
#> 975                           پنجابی
#> 976                         Ποντιακά
#> 977                                 
#> 978                                 
#> 979                            Nawat
#> 980                                 
#> 981                                 
#> 982                                 
#> 983                                 
#> 984                        prūsiskan
#> 985                                 
#> 986                                 
#> 987                             پښتو
#> 988                                 
#> 989                                 
#> 990                                 
#> 991                                 
#> 992                                 
#> 993                                 
#> 994                                 
#> 995                                 
#> 996                                 
#> 997                        português
#> 998                                 
#> 999              português do Brasil
#> 1000                                
#> 1001                                
#> 1002                      pinayuanan
#> 1003                                
#> 1004                                
#> 1005                       Runa Simi
#> 1006                                
#> 1007                      Runa shimi
#> 1008                                
#> 1009                                
#> 1010                                
#> 1011                                
#> 1012                                
#> 1013                                
#> 1014                                
#> 1015                                
#> 1016                                
#> 1017                                
#> 1018                                
#> 1019                        Rumagnôl
#> 1020                                
#> 1021                                
#> 1022                                
#> 1023                         Tarifit
#> 1024                             ရခိုင်
#> 1025                                
#> 1026                       rumantsch
#> 1027                                
#> 1028                                
#> 1029                                
#> 1030                                
#> 1031                                
#> 1032                                
#> 1033                     romaňi čhib
#> 1034                                
#> 1035                                
#> 1036                                
#> 1037                                
#> 1038                                
#> 1039                                
#> 1040                                
#> 1041                     romani čhib
#> 1042                        ikirundi
#> 1043                          română
#> 1044                                
#> 1045                                
#> 1046                     armãneashti
#> 1047                       tarandíne
#> 1048                                
#> 1049                                
#> 1050                           руски
#> 1051                                
#> 1052                         русский
#> 1053                                
#> 1054                      русиньскый
#> 1055                                
#> 1056                                
#> 1057                     armãneashti
#> 1058                        Vlăheşte
#> 1059                        Влахесте
#> 1060                        Vlăheşte
#> 1061                      мыхаӀбишды
#> 1062                    Ikinyarwanda
#> 1063                                
#> 1064                                
#> 1065                                
#> 1066                                
#> 1067                    うちなーぐち
#> 1068                                
#> 1069                           संस्कृतम्
#> 1070                                
#> 1071                                
#> 1072                       саха тыла
#> 1073                                
#> 1074                                
#> 1075                                
#> 1076                                
#> 1077                           Sasak
#> 1078                         ᱥᱟᱱᱛᱟᱲᱤ
#> 1079                                
#> 1080                                
#> 1081                                
#> 1082                                
#> 1083                                
#> 1084                                
#> 1085                           sardu
#> 1086                                
#> 1087                       sicilianu
#> 1088                           Scots
#> 1089                            سنڌي
#> 1090                                
#> 1091                                
#> 1092                                
#> 1093                                
#> 1094                       Sassaresu
#> 1095                     کوردی خوارگ
#> 1096                                
#> 1097                                
#> 1098                                
#> 1099                 davvisámegiella
#> 1100  davvisámegiella (Suoma bealde)
#> 1101 davvisámegiella (Norgga bealde)
#> 1102  davvisámegiella (Ruoŧa bealde)
#> 1103                                
#> 1104                                
#> 1105                                
#> 1106                     Cmique Itom
#> 1107                                
#> 1108                                
#> 1109                                
#> 1110                 Koyraboro Senni
#> 1111                           Sängö
#> 1112                                
#> 1113                                
#> 1114                                
#> 1115                                
#> 1116                                
#> 1117                                
#> 1118                      žemaitėška
#> 1119                                
#> 1120                                
#> 1121 srpskohrvatski / српскохрватски
#> 1122       српскохрватски (ћирилица)
#> 1123       srpskohrvatski (latinica)
#> 1124                                
#> 1125                         Taclḥit
#> 1126                         Taclḥit
#> 1127                         ⵜⴰⵛⵍⵃⵉⵜ
#> 1128                              တႆး
#> 1129                                
#> 1130                         tacawit
#> 1131                                
#> 1132                         tacawit
#> 1133                                
#> 1134                            සිංහල
#> 1135                                
#> 1136                                
#> 1137                  Simple English
#> 1138                                
#> 1139                                
#> 1140                 кӣллт са̄мь кӣлл
#> 1141                 bidumsámegiella
#> 1142                                
#> 1143                                
#> 1144                                
#> 1145                                
#> 1146                                
#> 1147                      slovenčina
#> 1148                         سرائیکی
#> 1149                         سرائیکی
#> 1150                     slovenščina
#> 1151                                
#> 1152                                
#> 1153                        Schläsch
#> 1154                                
#> 1155                                
#> 1156                    Gagana Samoa
#> 1157                   åarjelsaemien
#> 1158                                
#> 1159                                
#> 1160                     anarâškielâ
#> 1161                nuõrttsääʹmǩiõll
#> 1162                        chiShona
#> 1163                                
#> 1164                                
#> 1165                      Soomaaliga
#> 1166                                
#> 1167                                
#> 1168                                
#> 1169                           shqip
#> 1170                 српски / srpski
#> 1171               српски (ћирилица)
#> 1172               српски (ћирилица)
#> 1173               srpski (latinica)
#> 1174               srpski (latinica)
#> 1175                                
#> 1176                                
#> 1177                                
#> 1178                                
#> 1179                                
#> 1180                     Sranantongo
#> 1181               sardu campidanesu
#> 1182                                
#> 1183                                
#> 1184                         SiSwati
#> 1185                                
#> 1186                                
#> 1187                                
#> 1188                                
#> 1189                         Sesotho
#> 1190                                
#> 1191                       Seeltersk
#> 1192                                
#> 1193                      себертатар
#> 1194                           Sunda
#> 1195                                
#> 1196                                
#> 1197                                
#> 1198                                
#> 1199                                
#> 1200                                
#> 1201                         svenska
#> 1202                                
#> 1203                       Kiswahili
#> 1204                                
#> 1205                                
#> 1206                                
#> 1207                                
#> 1208                                
#> 1209                           ꠍꠤꠟꠐꠤ
#> 1210                                
#> 1211                                
#> 1212                                
#> 1213                         ślůnski
#> 1214                        Sakizaya
#> 1215                            தமிழ்
#> 1216                                
#> 1217                                
#> 1218                           Tayal
#> 1219                                
#> 1220                                
#> 1221                            ತುಳು
#> 1222                    ᥖᥭᥰ ᥖᥬᥲ ᥑᥨᥒᥰ
#> 1223                           తెలుగు
#> 1224                                
#> 1225                                
#> 1226                                
#> 1227                           tetun
#> 1228                          тоҷикӣ
#> 1229                          тоҷикӣ
#> 1230                          tojikī
#> 1231                                
#> 1232                             ไทย
#> 1233                                
#> 1234                                
#> 1235                                
#> 1236                            ትግርኛ
#> 1237                             ትግሬ
#> 1238                                
#> 1239                                
#> 1240                                
#> 1241                       Türkmençe
#> 1242                                
#> 1243                                
#> 1244                         Tagalog
#> 1245                                
#> 1246                                
#> 1247                                
#> 1248                                
#> 1249                                
#> 1250                          tolışi
#> 1251                          толыши
#> 1252                                
#> 1253                                
#> 1254                        Setswana
#> 1255                                
#> 1256                  lea faka-Tonga
#> 1257                                
#> 1258                                
#> 1259                       toki pona
#> 1260                       Tok Pisin
#> 1261                          Türkçe
#> 1262                                
#> 1263                          Ṫuroyo
#> 1264                          Seediq
#> 1265                                
#> 1266                        Xitsonga
#> 1267                                
#> 1268                                
#> 1269                                
#> 1270                                
#> 1271               татарча / tatarça
#> 1272                         татарча
#> 1273                         tatarça
#> 1274                        Orutooro
#> 1275                                
#> 1276                                
#> 1277                                
#> 1278                      chiTumbuka
#> 1279                                
#> 1280                                
#> 1281                                
#> 1282                                
#> 1283                             Twi
#> 1284                                
#> 1285                                
#> 1286                                
#> 1287                                
#> 1288                                
#> 1289                                
#> 1290                                
#> 1291                      reo tahiti
#> 1292                        тыва дыл
#> 1293                                
#> 1294                        ⵜⴰⵎⴰⵣⵉⵖⵜ
#> 1295                          удмурт
#> 1296            ئۇيغۇرچە / Uyghurche
#> 1297                        ئۇيغۇرچە
#> 1298                                
#> 1299                       Uyghurche
#> 1300                                
#> 1301                      українська
#> 1302                                
#> 1303                                
#> 1304                                
#> 1305                                
#> 1306                                
#> 1307                                
#> 1308                                
#> 1309                                
#> 1310                            اردو
#> 1311                                
#> 1312                                
#> 1313                                
#> 1314             oʻzbekcha / ўзбекча
#> 1315                         ўзбекча
#> 1316                       oʻzbekcha
#> 1317                                
#> 1318                       Tshivenda
#> 1319                          vèneto
#> 1320                     vepsän kel’
#> 1321                      Tiếng Việt
#> 1322                                
#> 1323                      West-Vlams
#> 1324                   Mainfränkisch
#> 1325                        emakhuwa
#> 1326                         Volapük
#> 1327                           Vaďďa
#> 1328                            võro
#> 1329                                
#> 1330                                
#> 1331                           walon
#> 1332                                
#> 1333                                
#> 1334                        wolaytta
#> 1335                         Winaray
#> 1336                                
#> 1337                                
#> 1338                                
#> 1339                                
#> 1340                                
#> 1341                                
#> 1342                                
#> 1343                                
#> 1344                                
#> 1345                                
#> 1346                                
#> 1347                       Fakaʻuvea
#> 1348                           waale
#> 1349                           Wolof
#> 1350                                
#> 1351                                
#> 1352                            吴语
#> 1353                    吴语（简体）
#> 1354                    吳語（正體）
#> 1355                                
#> 1356                                
#> 1357                          хальмг
#> 1358                                
#> 1359                        isiXhosa
#> 1360                       მარგალური
#> 1361                                
#> 1362                                
#> 1363                                
#> 1364                                
#> 1365                                
#> 1366                                
#> 1367                                
#> 1368                                
#> 1369                                
#> 1370                                
#> 1371                        saisiyat
#> 1372                                
#> 1373                                
#> 1374                                
#> 1375                                
#> 1376                                
#> 1377                                
#> 1378                                
#> 1379                                
#> 1380                                
#> 1381                                
#> 1382                                
#> 1383                                
#> 1384                                
#> 1385                           ייִדיש
#> 1386                                
#> 1387                          Yorùbá
#> 1388                                
#> 1389                                
#> 1390                                
#> 1391                                
#> 1392                                
#> 1393                                
#> 1394                        Nhẽẽgatú
#> 1395                     maaya t’aan
#> 1396                            粵語
#> 1397                    粵语（简体）
#> 1398                    粵語（繁體）
#> 1399                       Vahcuengh
#> 1400                                
#> 1401                                
#> 1402                                
#> 1403                          Zeêuws
#> 1404                                
#> 1405               ⵜⴰⵎⴰⵣⵉⵖⵜ ⵜⴰⵏⴰⵡⴰⵢⵜ
#> 1406               tamaziɣt tanawayt
#> 1407                            中文
#> 1408                            文言
#> 1409                中文（中国大陆）
#> 1410                    中文（简体）
#> 1411                    中文（繁體）
#> 1412                    中文（香港）
#> 1413             閩南語 / Bân-lâm-gí
#> 1414                    中文（澳門）
#> 1415                中文（马来西亚）
#> 1416                  中文（新加坡）
#> 1417                    中文（臺灣）
#> 1418                            粵語
#> 1419                                
#> 1420                                
#> 1421                                
#> 1422                         isiZulu
#> 1423                                
#> 1424                                
#> 1425                                
# }
```
