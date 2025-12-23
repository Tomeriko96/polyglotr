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
# Get language names
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
#> 17                 ae                                    Avestan
#> 18                aeb                            Tunisian Arabic
#> 19           aeb-arab            Tunisian Arabic (Arabic script)
#> 20           aeb-latn             Tunisian Arabic (Latin script)
#> 21                aec                              Saʽidi Arabic
#> 22                aee                           Northeast Pashai
#> 23                aer                           Eastern Arrernte
#> 24                 af                                  Afrikaans
#> 25                afh                                   Afrihili
#> 26                agq                                      Aghem
#> 27                ahr                                    Ahirani
#> 28                aig       Antiguan and Barbudan Creole English
#> 29                aii                       Assyrian Neo-Aramaic
#> 30                ain                                       Ainu
#> 31                ajg                                     Ajagbe
#> 32                 ak                                       Akan
#> 33                akb                              Batak Angkola
#> 34                akk                                   Akkadian
#> 35           akk-latn                    Akkadian (Latin script)
#> 36           akk-xsux                Akkadian (Cuneiform script)
#> 37                akz                                    Alabama
#> 38                alc                                   Kawésqar
#> 39                ale                                      Aleut
#> 40           ale-cyrl                    Aleut (Cyrillic script)
#> 41                aln                              Gheg Albanian
#> 42                alq                                  Algonquin
#> 43                als                                  Alemannic
#> 44                alt                             Southern Altai
#> 45                aly                                   Alyawarr
#> 46                 am                                    Amharic
#> 47                ami                                       Amis
#> 48                amx                                 Anmatyerre
#> 49                 an                                  Aragonese
#> 50                ane                                    Xârâcùù
#> 51                ang                                Old English
#> 52                ann                                      Obolo
#> 53                anp                                     Angika
#> 54                apc                           Levantine Arabic
#> 55           apc-latn            Levantine Arabic (Latin script)
#> 56                 ar                                     Arabic
#> 57             ar-001                     Modern Standard Arabic
#> 58                arc                                    Aramaic
#> 59                are                           Western Arrarnta
#> 60                arn                                    Mapuche
#> 61                aro                                     Araona
#> 62                arp                                    Arapaho
#> 63                arq                            Algerian Arabic
#> 64                ars                               Najdi Arabic
#> 65                arw                                     Arawak
#> 66                ary                            Moroccan Arabic
#> 67           ary-arab            Moroccan Arabic (Arabic script)
#> 68           ary-latn             Moroccan Arabic (Latin script)
#> 69                arz                            Egyptian Arabic
#> 70                 as                                   Assamese
#> 71                asa                                        Asu
#> 72                ase                     American Sign Language
#> 73                ast                                   Asturian
#> 74                atj                                  Atikamekw
#> 75                atv                             Northern Altai
#> 76                 av                                     Avaric
#> 77                avk                                     Kotava
#> 78                awa                                     Awadhi
#> 79                axe                                Ayerrerenge
#> 80                axl                      Lower Southern Aranda
#> 81                 ay                                     Aymara
#> 82                ayh                            Hadhrami Arabic
#> 83                 az                                Azerbaijani
#> 84            az-cyrl              Azerbaijani (Cyrillic script)
#> 85                azb                          South Azerbaijani
#> 86                 ba                                    Bashkir
#> 87                bag                                       Tuki
#> 88                bal                                    Baluchi
#> 89           bal-latn                     Baluchi (Latin script)
#> 90                ban                                   Balinese
#> 91           ban-bali                 Balinese (Balinese script)
#> 92                bar                                   Bavarian
#> 93                bas                                      Basaa
#> 94            bat-smg                                 Samogitian
#> 95                bax                                      Bamun
#> 96                bbc                                 Batak Toba
#> 97           bbc-latn                  Batak Toba (Latin script)
#> 98                bbj                                    Ghomala
#> 99                bcc                           Southern Balochi
#> 100               bci                                     Baoulé
#> 101               bcl                              Central Bikol
#> 102               bdr                           West Coast Bajau
#> 103                be                                 Belarusian
#> 104         be-tarask      Belarusian (Taraškievica orthography)
#> 105          be-x-old      Belarusian (Taraškievica orthography)
#> 106               bej                                       Beja
#> 107               bem                                      Bemba
#> 108               bew                                     Betawi
#> 109               bez                                       Bena
#> 110               bfa                                       Bari
#> 111               bfd                                      Bafut
#> 112               bfi                      British Sign Language
#> 113               bfq                                     Badaga
#> 114               bft                                      Balti
#> 115          bft-tibt                     Balti (Tibetan script)
#> 116               bfw                                      Bonda
#> 117               bfz                              Mahasu Pahari
#> 118          bfz-deva          Mahasu Pahari (Devanagari script)
#> 119          bfz-takr               Mahasu Pahari (Takri script)
#> 120                bg                                  Bulgarian
#> 121               bgc                                   Haryanvi
#> 122          bgc-arab                   Haryanvi (Arabic script)
#> 123          bgc-deva               Haryanvi (Devanagari script)
#> 124               bgn                            Western Balochi
#> 125               bgp                            Eastern Balochi
#> 126               bgq                                      Bagri
#> 127          bgq-arab                      Bagri (Arabic script)
#> 128          bgq-deva                  Bagri (Devanagari script)
#> 129                bh                                   Bhojpuri
#> 130               bha                                     Bharia
#> 131               bhd                                 Bhadrawahi
#> 132          bhd-deva             Bhadrawahi (Devanagari script)
#> 133          bhd-takr                  Bhadrawahi (Takri script)
#> 134               bho                                   Bhojpuri
#> 135                bi                                    Bislama
#> 136               bik                                      Bikol
#> 137               bin                                       Bini
#> 138               bjn                                     Banjar
#> 139               bkc                                       Baka
#> 140               bkh                                     Bakoko
#> 141               bkm                                        Kom
#> 142               bla                                    Siksiká
#> 143               blc                                     Nuxalk
#> 144               blk                                       Pa'O
#> 145               blo                                       Anii
#> 146               blt                                    Tai Dam
#> 147                bm                                    Bambara
#> 148                bn                                     Bangla
#> 149               bnn                                      Bunun
#> 150                bo                                    Tibetan
#> 151               bol                                       Bole
#> 152               bom                                      Berom
#> 153               bpy                                Bishnupriya
#> 154               bqi                                  Bakhtiari
#> 155               bqz                                      Mka'a
#> 156                br                                     Breton
#> 157               bra                                       Braj
#> 158               brh                                     Brahui
#> 159          brh-latn                      Brahui (Latin script)
#> 160               brx                                       Bodo
#> 161                bs                                    Bosnian
#> 162               bse                                      Wushi
#> 163               bsk                                 Burushaski
#> 164               bss                                     Akoose
#> 165               btd                                Batak Dairi
#> 166               btm                           Batak Mandailing
#> 167               bto                            Rinconada Bikol
#> 168               bts                           Batak Simalungun
#> 169               btx                                 Batak Karo
#> 170               btz                           Batak Alas-Kluet
#> 171               bua                                     Buriat
#> 172               bug                                   Buginese
#> 173          bug-bugi                 Buginese (Buginese script)
#> 174               bum                                       Bulu
#> 175               bvb                                       Bube
#> 176               bwr                                 Bura-Pabir
#> 177               bxr                              Russia Buriat
#> 178               byn                                       Blin
#> 179               byv                                    Medumba
#> 180               bzj                               Belize Kriol
#> 181               bzs                    Brazilian Sign Language
#> 182                ca                                    Catalan
#> 183               cad                                      Caddo
#> 184               cak                                  Kaqchikel
#> 185               cal                                 Carolinian
#> 186               car                                      Carib
#> 187               cay                                     Cayuga
#> 188           cbk-zam                                  Chavacano
#> 189               cch                                      Atsam
#> 190               ccp                                     Chakma
#> 191          ccp-beng                    Chakma (Bengali script)
#> 192               cdo                                    Mindong
#> 193          cdo-hani                       Mindong (Han script)
#> 194          cdo-hant           Mindong (Traditional Han script)
#> 195          cdo-latn                     Mindong (Latin script)
#> 196          cdz-beng                      Koda (Bengali script)
#> 197                ce                                    Chechen
#> 198               ceb                                    Cebuano
#> 199               cgg                                      Chiga
#> 200                ch                                   Chamorro
#> 201               chb                                    Chibcha
#> 202               chg                                   Chagatai
#> 203               chk                                   Chuukese
#> 204               chm                                       Mari
#> 205               chn                             Chinook Jargon
#> 206               cho                                    Choctaw
#> 207               chp                                  Chipewyan
#> 208               chr                                   Cherokee
#> 209               chy                                   Cheyenne
#> 210               cic                                  Chickasaw
#> 211               cja                               Western Cham
#> 212          cja-arab               Western Cham (Arabic script)
#> 213          cja-cham                 Western Cham (Cham script)
#> 214          cja-latn                Western Cham (Latin script)
#> 215               cjm                               Eastern Cham
#> 216          cjm-arab               Eastern Cham (Arabic script)
#> 217          cjm-cham                 Eastern Cham (Cham script)
#> 218          cjm-latn                Eastern Cham (Latin script)
#> 219               cjy                                        Jin
#> 220          cjy-hans                Jin (Simplified Han script)
#> 221          cjy-hant               Jin (Traditional Han script)
#> 222               ckb                            Central Kurdish
#> 223          ckb-arab            Central Kurdish (Arabic script)
#> 224          ckb-latn             Central Kurdish (Latin script)
#> 225               cko                                      Anufo
#> 226               ckt                                    Chukchi
#> 227               ckv                                    Kavalan
#> 228               clc                                  Chilcotin
#> 229               cmg                        Classical Mongolian
#> 230               cnh                                 Hakha-Chin
#> 231               cnr                                Montenegrin
#> 232          cnr-cyrl              Montenegrin (Cyrillic script)
#> 233          cnr-latn                 Montenegrin (Latin script)
#> 234               cnx                             Middle Cornish
#> 235                co                                   Corsican
#> 236               cop                                     Coptic
#> 237               cps                                   Capiznon
#> 238               cpx                                     Puxian
#> 239          cpx-hans             Puxian (Simplified Han script)
#> 240          cpx-hant            Puxian (Traditional Han script)
#> 241          cpx-latn                      Puxian (Latin script)
#> 242                cr                                       Cree
#> 243               crb                               Island Carib
#> 244               crg                                     Michif
#> 245               crh                              Crimean Tatar
#> 246          crh-cyrl            Crimean Tatar (Cyrillic script)
#> 247          crh-latn               Crimean Tatar (Latin script)
#> 248            crh-ro                             Dobrujan Tatar
#> 249               crj                         Southern East Cree
#> 250               crk                                Plains Cree
#> 251               crl                         Northern East Cree
#> 252               crm                                 Moose Cree
#> 253               crr                        Carolina Algonquian
#> 254               crs                      Seselwa Creole French
#> 255                cs                                      Czech
#> 256               csb                                  Kashubian
#> 257               csw                                Swampy Cree
#> 258               ctg                               Chittagonian
#> 259                cu                              Church Slavic
#> 260                cv                                    Chuvash
#> 261                cy                                      Welsh
#> 262                da                                     Danish
#> 263               dag                                    Dagbani
#> 264               dak                                     Dakota
#> 265               dar                                     Dargwa
#> 266               dav                                      Taita
#> 267               ddn                                      Dendi
#> 268                de                                     German
#> 269           de-1901           German (traditional orthography)
#> 270             de-at                            Austrian German
#> 271             de-ch                          Swiss High German
#> 272         de-formal                    German (formal address)
#> 273               del                                   Delaware
#> 274               den                                      Slave
#> 275               dga                           Southern Dagaare
#> 276               dgr                                     Dogrib
#> 277               din                                      Dinka
#> 278               diq                                      Dimli
#> 279               dje                                      Zarma
#> 280               dlg                                     Dolgan
#> 281               doi                                      Dogri
#> 282          doi-arab                      Dogri (Arabic script)
#> 283          doi-deva                  Dogri (Devanagari script)
#> 284          doi-dogr                       Dogri (Dogra script)
#> 285               dru                                      Rukai
#> 286               dsb                              Lower Sorbian
#> 287               dso                                     Desiya
#> 288               dtp                              Central Dusun
#> 289               dty                                     Doteli
#> 290               dua                                      Duala
#> 291               duf                                     Dumbea
#> 292               dum                               Middle Dutch
#> 293                dv                                     Divehi
#> 294               dyo                                 Jola-Fonyi
#> 295               dyu                                      Dyula
#> 296                dz                                   Dzongkha
#> 297               dzg                                     Dazaga
#> 298               ebu                                       Embu
#> 299                ee                                        Ewe
#> 300               efi                                       Efik
#> 301               egl                         Emiliano-Romagnolo
#> 302               egy                           Ancient Egyptian
#> 303               eka                                     Ekajuk
#> 304               ekp                                     Ekpeye
#> 305                el                                      Greek
#> 306             el-cy                              Cypriot Greek
#> 307               elm                                      Eleme
#> 308               elx                                    Elamite
#> 309               eml                         Emiliano-Romagnolo
#> 310                en                                    English
#> 311             en-au                         Australian English
#> 312             en-ca                           Canadian English
#> 313             en-gb                            British English
#> 314             en-in                             Indian English
#> 315             en-jm                           Jamaican English
#> 316             en-nz                        New Zealand English
#> 317             en-us                           American English
#> 318               enm                             Middle English
#> 319                eo                                  Esperanto
#> 320       eo-hsistemo           Esperanto (h-system orthography)
#> 321       eo-xsistemo           Esperanto (x-system orthography)
#> 322                es                                    Spanish
#> 323            es-419                     Latin American Spanish
#> 324             es-es                           European Spanish
#> 325         es-formal                   Spanish (formal address)
#> 326             es-mx                            Mexican Spanish
#> 327               ess                     Central Siberian Yupik
#> 328               esu                              Central Yupik
#> 329                et                                   Estonian
#> 330               eto                                       Eton
#> 331               ett                                   Etruscan
#> 332               etu                                    Ejagham
#> 333                eu                                     Basque
#> 334               ewo                                     Ewondo
#> 335               ext                               Extremaduran
#> 336               eya                                       Eyak
#> 337                fa                                    Persian
#> 338             fa-af                                       Dari
#> 339               fan                                       Fang
#> 340               fat                                      Fanti
#> 341                ff                                       Fula
#> 342                fi                                    Finnish
#> 343               fil                                   Filipino
#> 344               fit                         Tornedalen Finnish
#> 345           fiu-vro                                       Võro
#> 346                fj                                     Fijian
#> 347               fkv                                     Kvensk
#> 348               fmp                                     Fe'Fe'
#> 349                fo                                    Faroese
#> 350               fon                                        Fon
#> 351               fos                                     Siraya
#> 352                fr                                     French
#> 353             fr-ca                            Canadian French
#> 354             fr-ch                               Swiss French
#> 355               frc                               Cajun French
#> 356               frk                                   Frankish
#> 357               frm                              Middle French
#> 358               fro                                 Old French
#> 359               frp                                    Arpitan
#> 360               frr                           Northern Frisian
#> 361               frs                            Eastern Frisian
#> 362               fsl                       French Sign Language
#> 363               fud                                    Futunan
#> 364               fuf                                      Pular
#> 365               fur                                   Friulian
#> 366               fvr                                        Fur
#> 367                fy                            Western Frisian
#> 368                ga                                      Irish
#> 369               gaa                                         Ga
#> 370               gag                                     Gagauz
#> 371               gan                                        Gan
#> 372          gan-hans                Gan (Simplified Han script)
#> 373          gan-hant               Gan (Traditional Han script)
#> 374               gay                                       Gayo
#> 375               gba                                      Gbaya
#> 376               gbb                                   Kaytetye
#> 377               gbk                                      Gaddi
#> 378          gbk-deva                  Gaddi (Devanagari script)
#> 379          gbk-takr                       Gaddi (Takri script)
#> 380               gbm                                   Garhwali
#> 381               gbz                           Zoroastrian Dari
#> 382               gcf                        Guadeloupean Creole
#> 383               gcr                             Guianan Creole
#> 384                gd                            Scottish Gaelic
#> 385               gez                                       Geez
#> 386               gil                                 Gilbertese
#> 387               gju                                     Gujari
#> 388          gju-arab                     Gujari (Arabic script)
#> 389          gju-deva                 Gujari (Devanagari script)
#> 390                gl                                   Galician
#> 391               gld                                      Nanai
#> 392               glh                           Northwest Pashai
#> 393               glk                                     Gilaki
#> 394               gmh                         Middle High German
#> 395               gml                          Middle Low German
#> 396               gmy                            Mycenaean Greek
#> 397                gn                                    Guarani
#> 398               goh                            Old High German
#> 399               gom                               Goan Konkani
#> 400          gom-deva           Goan Konkani (Devanagari script)
#> 401          gom-latn                Goan Konkani (Latin script)
#> 402               gon                                      Gondi
#> 403               gor                                  Gorontalo
#> 404               got                                     Gothic
#> 405               gpe                            Ghanaian Pidgin
#> 406               grb                                      Grebo
#> 407               grc                              Ancient Greek
#> 408               gsg                       German Sign Language
#> 409               gsw                                  Alemannic
#> 410            gsw-fr                                   Alsatian
#> 411                gu                                   Gujarati
#> 412               guc                                      Wayuu
#> 413               gur                                     Frafra
#> 414               guw                                        Gun
#> 415               guz                                      Gusii
#> 416                gv                                       Manx
#> 417               gwi                                   Gwichʼin
#> 418               gya                                      Gbaya
#> 419                ha                                      Hausa
#> 420           ha-arab                      Hausa (Arabic script)
#> 421             ha-ne                              Hausa (Niger)
#> 422               hac                                     Gurani
#> 423               hai                                      Haida
#> 424               hak                              Hakka Chinese
#> 425          hak-hans              Hakka (Simplified Han script)
#> 426          hak-hant             Hakka (Traditional Han script)
#> 427          hak-latn                       Hakka (Latin script)
#> 428               hav                                       Havu
#> 429               haw                                   Hawaiian
#> 430               hax                             Southern Haida
#> 431               haz                                   Hazaragi
#> 432               hbo                            Biblical Hebrew
#> 433                he                                     Hebrew
#> 434                hi                                      Hindi
#> 435           hi-kthi                      Hindi (Kaithi script)
#> 436           hi-latn                              Hindi (Latin)
#> 437               hif                                 Fiji Hindi
#> 438          hif-deva             Fiji Hindi (Devanagari script)
#> 439          hif-latn                  Fiji Hindi (Latin script)
#> 440               hil                                 Hiligaynon
#> 441               hit                                    Hittite
#> 442          hit-latn                     Hittite (Latin script)
#> 443          hit-xsux                 Hittite (Cuneiform script)
#> 444               hke                                      Hunde
#> 445               hmn                                      Hmong
#> 446               hne                              Chhattisgarhi
#> 447               hnj                                 Hmong Njua
#> 448               hno                            Northern Hindko
#> 449                ho                                  Hiri Motu
#> 450               hoc                                         Ho
#> 451          hoc-latn                          Ho (Latin script)
#> 452                hr                                   Croatian
#> 453               hrx                                    Hunsrik
#> 454               hsb                              Upper Sorbian
#> 455               hsn                                      Xiang
#> 456          hsn-hans              Xiang (Simplified Han script)
#> 457          hsn-hant             Xiang (Traditional Han script)
#> 458                ht                             Haitian Creole
#> 459               hts                                      Hadza
#> 460                hu                                  Hungarian
#> 461         hu-formal                 Hungarian (formal address)
#> 462               hup                                       Hupa
#> 463               hur                                 Halkomelem
#> 464                hy                                   Armenian
#> 465               hyw                           Western Armenian
#> 466                hz                                     Herero
#> 467                ia                                Interlingua
#> 468               iba                                       Iban
#> 469               ibb                                     Ibibio
#> 470                id                                 Indonesian
#> 471                ie                                Interlingue
#> 472                ig                                       Igbo
#> 473               igb                                      Ebira
#> 474               igl                                      Igala
#> 475                ii                                 Sichuan Yi
#> 476                ik                                    Inupiaq
#> 477          ike-cans    Eastern Canadian (Aboriginal syllabics)
#> 478          ike-latn            Eastern Canadian (Latin script)
#> 479               ikt                 Western Canadian Inuktitut
#> 480               ilo                                      Iloko
#> 481               inh                                     Ingush
#> 482                io                                        Ido
#> 483                is                                  Icelandic
#> 484               ish                                       Esan
#> 485               isu                                        Isu
#> 486          isv-cyrl              Interslavic (Cyrillic script)
#> 487          isv-latn                 Interslavic (Latin script)
#> 488                it                                    Italian
#> 489                iu                                  Inuktitut
#> 490               izh                                    Ingrian
#> 491                ja                                   Japanese
#> 492           ja-hani                    Japanese (Kanji script)
#> 493           ja-hira                 Japanese (Hiragana script)
#> 494           ja-hrkt                     Japanese (Kana script)
#> 495           ja-kana                 Japanese (Katakana script)
#> 496               jac                                     Popti'
#> 497               jam                    Jamaican Creole English
#> 498               jbo                                     Lojban
#> 499               jdt                                  Judeo-Tat
#> 500               jgo                                     Ngomba
#> 501               jje                                       Jeju
#> 502               jmc                                    Machame
#> 503               jpr                              Judeo-Persian
#> 504               jrb                               Judeo-Arabic
#> 505               jut                                     Jutish
#> 506                jv                                   Javanese
#> 507           jv-java                 Javanese (Javanese script)
#> 508                ka                                   Georgian
#> 509               kaa                                Kara-Kalpak
#> 510               kab                                     Kabyle
#> 511               kac                                     Kachin
#> 512               kai                                   Karekare
#> 513               kaj                                        Jju
#> 514               kam                                      Kamba
#> 515               kaw                                       Kawi
#> 516               kbd                                  Kabardian
#> 517          kbd-cyrl                Kabardian (Cyrillic script)
#> 518               kbl                                    Kanembu
#> 519               kbp                                     Kabiye
#> 520               kcg                                       Tyap
#> 521               kck                                    Kalanga
#> 522               kde                                    Makonde
#> 523               kea                               Kabuverdianu
#> 524               ken                                    Kenyang
#> 525               ker                                       Kera
#> 526               kfo                                       Koro
#> 527               kfr                                     Kutchi
#> 528                kg                                      Kongo
#> 529               kge                                   Komering
#> 530          kge-arab                   Komering (Arabic script)
#> 531               kgg                                    Kusunda
#> 532               kgp                                   Kaingang
#> 533               kha                                      Khasi
#> 534               kho                                  Khotanese
#> 535               khq                               Koyra Chiini
#> 536               khw                                     Khowar
#> 537                ki                                     Kikuyu
#> 538               kip                                Sheshi Kham
#> 539               kiu                                  Kirmanjki
#> 540                kj                                   Kuanyama
#> 541               kjh                                     Khakas
#> 542               kjp                                Eastern Pwo
#> 543                kk                                     Kazakh
#> 544           kk-arab                     Kazakh (Arabic script)
#> 545             kk-cn                             Kazakh (China)
#> 546           kk-cyrl                   Kazakh (Cyrillic script)
#> 547             kk-kz                        Kazakh (Kazakhstan)
#> 548           kk-latn                      Kazakh (Latin script)
#> 549             kk-tr                            Kazakh (Turkey)
#> 550               kkj                                       Kako
#> 551                kl                                Kalaallisut
#> 552               kld                                 Gamilaraay
#> 553               kln                                   Kalenjin
#> 554               kls                                    Kalasha
#> 555          kls-arab                    Kalasha (Arabic script)
#> 556          kls-latn                     Kalasha (Latin script)
#> 557                km                                      Khmer
#> 558               kmb                                   Kimbundu
#> 559               kmr                           Northern Kurdish
#> 560          kmr-arab           Northern Kurdish (Arabic script)
#> 561          kmr-latn            Northern Kurdish (Latin script)
#> 562               kmz                           Khorasani Turkic
#> 563                kn                                    Kannada
#> 564               knc                             Central Kanuri
#> 565               kne                                  Kankanaey
#> 566               knn                      Maharashtrian Konkani
#> 567                ko                                     Korean
#> 568             ko-cn                             Korean (China)
#> 569           ko-hani                      Korean (Hanja script)
#> 570           ko-kore                      Korean (mixed script)
#> 571             ko-kp                       Korean (North Korea)
#> 572               koi                               Komi-Permyak
#> 573               kok                                    Konkani
#> 574               kos                                   Kosraean
#> 575               koy                                    Koyukon
#> 576               kpe                                     Kpelle
#> 577                kr                                     Kanuri
#> 578               krc                            Karachay-Balkar
#> 579               kri                                       Krio
#> 580               krj                                  Kinaray-a
#> 581               krl                                   Karelian
#> 582               kru                                     Kurukh
#> 583                ks                                   Kashmiri
#> 584           ks-arab                   Kashmiri (Arabic script)
#> 585           ks-deva               Kashmiri (Devanagari script)
#> 586               ksb                                   Shambala
#> 587               ksf                                      Bafia
#> 588               ksh                                  Colognian
#> 589               ksw                                S'gaw Karen
#> 590          ksy-beng               Kharia Thar (Bengali script)
#> 591                ku                                    Kurdish
#> 592           ku-arab                    Kurdish (Arabic script)
#> 593           ku-latn                     Kurdish (Latin script)
#> 594               kum                                      Kumyk
#> 595               kus                                     Kusaal
#> 596               kut                                    Kutenai
#> 597                kv                                       Komi
#> 598                kw                                    Cornish
#> 599               kwk                                  Kwakʼwala
#> 600               kxv                                       Kuvi
#> 601                ky                                     Kyrgyz
#> 602          kyw-beng                   Kurmali (Bengali script)
#> 603          kyw-deva                Kurmali (Devanagari script)
#> 604                la                                      Latin
#> 605               lad                                     Ladino
#> 606          lad-hebr                     Ladino (Hebrew script)
#> 607               lag                                      Langi
#> 608               lah                            Western Panjabi
#> 609               laj                                      Lango
#> 610               lam                                      Lamba
#> 611                lb                              Luxembourgish
#> 612               lbe                                        Lak
#> 613               lcm                                     Tungag
#> 614               ldn                                     Láadan
#> 615               lem                                   Nomaande
#> 616               lez                                   Lezghian
#> 617               lfn                         Lingua Franca Nova
#> 618                lg                                      Ganda
#> 619                li                                 Limburgish
#> 620               lij                                   Ligurian
#> 621            lij-mc                                 Monégasque
#> 622               lil                                   Lillooet
#> 623               liv                                   Livonian
#> 624               ljp                                Lampung Api
#> 625               lki                                       Laki
#> 626               lkt                                     Lakota
#> 627               lld                                      Ladin
#> 628               lmn                                    Lambadi
#> 629               lmo                                    Lombard
#> 630                ln                                    Lingala
#> 631               lns                                    Lamnso'
#> 632                lo                                        Lao
#> 633               lol                                      Mongo
#> 634               lou                           Louisiana Creole
#> 635               loz                                       Lozi
#> 636               lrc                              Northern Luri
#> 637               lsm                                     Saamia
#> 638                lt                                 Lithuanian
#> 639               ltg                                  Latgalian
#> 640                lu                               Luba-Katanga
#> 641               lua                                 Luba-Lulua
#> 642               lui                                    Luiseno
#> 643               lun                                      Lunda
#> 644               luo                                        Luo
#> 645               lus                                       Mizo
#> 646               luy                                      Luyia
#> 647               luz                              Southern Luri
#> 648                lv                                    Latvian
#> 649               lzh                           Literary Chinese
#> 650               lzz                                        Laz
#> 651               mad                                   Madurese
#> 652               maf                                       Mafa
#> 653               mag                                     Magahi
#> 654               mai                                   Maithili
#> 655               mak                                    Makasar
#> 656          mak-bugi                  Makasar (Buginese script)
#> 657               man                                   Mandingo
#> 658           map-bms                                 Banyumasan
#> 659               mas                                      Masai
#> 660               maw                                   Mampruli
#> 661               mcn                                      Massa
#> 662               mcp                                       Maka
#> 663               mde                                       Maba
#> 664               mdf                                     Moksha
#> 665               mdh                               Maguindanaon
#> 666               mdr                                     Mandar
#> 667               men                                      Mende
#> 668               mer                                       Meru
#> 669               mey                                 Hassaniyya
#> 670               mfa                     Kelantan-Pattani Malay
#> 671               mfe                                   Morisyen
#> 672                mg                                   Malagasy
#> 673               mga                               Middle Irish
#> 674               mgh                             Makhuwa-Meetto
#> 675               mgo                                      Metaʼ
#> 676                mh                                Marshallese
#> 677               mhk                                    Mungaka
#> 678               mhr                               Eastern Mari
#> 679                mi                                      Māori
#> 680               mic                                    Mi'kmaw
#> 681               mid                                    Mandaic
#> 682               min                                Minangkabau
#> 683               mis                       unsupported language
#> 684               mix                                     Mixtec
#> 685          mjx-beng                    Mahali (Bengali script)
#> 686                mk                                 Macedonian
#> 687                ml                                  Malayalam
#> 688                mn                                  Mongolian
#> 689           mn-mong               Mongolian (Mongolian script)
#> 690               mnc                                     Manchu
#> 691          mnc-latn                      Manchu (Latin script)
#> 692          mnc-mong                  Manchu (Mongolian script)
#> 693               mni                                   Manipuri
#> 694               mns                                      Mansi
#> 695               mnw                                        Mon
#> 696                mo                                   Moldovan
#> 697               moe                                 Innu-aimun
#> 698               moh                                     Mohawk
#> 699               mos                                      Mossi
#> 700                mr                                    Marathi
#> 701               mrh                                       Mara
#> 702               mrj                               Western Mari
#> 703               mrt                             Marghi Central
#> 704               mrv                                  Mangareva
#> 705                ms                                      Malay
#> 706           ms-arab                        Malay (Jawi script)
#> 707               msi                                Sabah Malay
#> 708                mt                                    Maltese
#> 709               mua                                    Mundang
#> 710               mui                                       Musi
#> 711               mul                         multiple languages
#> 712               mus                                   Muscogee
#> 713               mvf                       Peripheral Mongolian
#> 714               mvi                                     Miyako
#> 715          mvi-hira                   Miyako (Hiragana script)
#> 716               mwl                                  Mirandese
#> 717               mwr                                    Marwari
#> 718               mwv                                   Mentawai
#> 719               mww                                  Hmong Daw
#> 720          mww-latn                   Hmong Daw (Latin script)
#> 721                my                                    Burmese
#> 722               mye                                      Myene
#> 723               myv                                      Erzya
#> 724               mzn                                Mazanderani
#> 725                na                                      Nauru
#> 726               nah                                    Nahuatl
#> 727               nan                                     Minnan
#> 728          nan-hani                        Minnan (Han script)
#> 729          nan-hans             Minnan (Simplified Han script)
#> 730          nan-hant            Minnan (Traditional Han script)
#> 731  nan-latn-pehoeji                         Minnan (Pe̍h-ōe-jī)
#> 732    nan-latn-tailo                            Minnan (Tâi-lô)
#> 733               nap                                 Neapolitan
#> 734               naq                                       Nama
#> 735                nb                           Norwegian Bokmål
#> 736                nd                              North Ndebele
#> 737               nds                                 Low German
#> 738            nds-nl                                  Low Saxon
#> 739                ne                                     Nepali
#> 740               new                                     Newari
#> 741                ng                                     Ndonga
#> 742               nge                                     Ngémba
#> 743               nia                                       Nias
#> 744               nit                        Southeastern Kolami
#> 745               niu                                     Niuean
#> 746               njo                                    Ao Naga
#> 747                nl                                      Dutch
#> 748             nl-be                                    Flemish
#> 749       nl-informal                   Dutch (informal address)
#> 750               nla                                   Ngombala
#> 751               nmg                                     Kwasio
#> 752               nmz                                      Nawdm
#> 753                nn                          Norwegian Nynorsk
#> 754       nn-hognorsk                         Norwegian Høgnorsk
#> 755               nnh                                  Ngiemboon
#> 756               nnz                                   Nda'Nda'
#> 757                no                                  Norwegian
#> 758               nod                              Northern Thai
#> 759          nod-thai                Northern Thai (Thai script)
#> 760               nog                                      Nogai
#> 761               non                                  Old Norse
#> 762          non-runr                   Old Norse (Runic script)
#> 763               nov                                     Novial
#> 764               nqo                                       N’Ko
#> 765                nr                              South Ndebele
#> 766            nrf-gg                                Guernésiais
#> 767            nrf-je                                   Jèrriais
#> 768               nrm                                     Norman
#> 769               nsk                                    Naskapi
#> 770               nso                             Northern Sotho
#> 771               nup                                       Nupe
#> 772               nus                                       Nuer
#> 773                nv                                     Navajo
#> 774               nwc                           Classical Newari
#> 775               nxm                                   Numidian
#> 776                ny                                     Nyanja
#> 777               nym                                   Nyamwezi
#> 778               nyn                                   Nyankole
#> 779               nyo                                      Nyoro
#> 780               nys                                    Nyungar
#> 781               nzi                                      Nzima
#> 782               obt                                 Old Breton
#> 783                oc                                    Occitan
#> 784               oco                                Old Cornish
#> 785               odt                                  Old Dutch
#> 786               ofs                                Old Frisian
#> 787                oj                                     Ojibwa
#> 788               ojb                        Northwestern Ojibwa
#> 789               ojc                             Central Ojibwa
#> 790               ojp                               Old Japanese
#> 791          ojp-hani                Old Japanese (Kanji script)
#> 792          ojp-hira             Old Japanese (Hiragana script)
#> 793               ojs                                   Oji-Cree
#> 794               ojw                             Western Ojibwa
#> 795               oka                                   Okanagan
#> 796               olo                             Livvi-Karelian
#> 797                om                                      Oromo
#> 798               oma                                Omaha-Ponca
#> 799               ood                                    O'odham
#> 800                or                                       Odia
#> 801                os                                    Ossetic
#> 802               osa                                      Osage
#> 803          osa-latn                       Osage (Latin script)
#> 804               osi                                      Osing
#> 805               osx                                  Old Saxon
#> 806               ota                            Ottoman Turkish
#> 807               otk                                Old Turkish
#> 808               ovd                                  Elfdalian
#> 809               owl                                  Old Welsh
#> 810                pa                                    Punjabi
#> 811               pag                                 Pangasinan
#> 812               pal                                    Pahlavi
#> 813          pal-phli     Pahlavi (Inscriptional Pahlavi script)
#> 814          pal-phlp           Pahlavi (Psalter Pahlavi script)
#> 815          pal-phlv              Pahlavi (Book Pahlavi script)
#> 816               pam                                   Pampanga
#> 817               pao                            Northern Paiute
#> 818               pap                                 Papiamento
#> 819            pap-aw                         Papiamento (Aruba)
#> 820               paq                                      Parya
#> 821               pau                                    Palauan
#> 822               pbb                                       Páez
#> 823               pcd                                     Picard
#> 824               pcm                            Nigerian Pidgin
#> 825               pdc                        Pennsylvania German
#> 826               pdt                               Plautdietsch
#> 827               peo                                Old Persian
#> 828               pfl                            Palatine German
#> 829               pgd                                   Gāndhārī
#> 830          pgd-arab                   Gāndhārī (Arabic script)
#> 831          pgd-deva               Gāndhārī (Devanagari script)
#> 832          pgd-khar               Gāndhārī (Kharoshthi script)
#> 833               phl                                     Palula
#> 834               phn                                 Phoenician
#> 835          phn-latn                  Phoenician (Latin script)
#> 836          phn-phnx             Phoenician (Phoenician script)
#> 837               phr                             Pahari-Potwari
#> 838                pi                                       Pali
#> 839           pi-sidd                      Pali (Siddham script)
#> 840               pih                           Pitcairn-Norfolk
#> 841               pis                                      Pijin
#> 842               pjt                             Pitjantjatjara
#> 843               pks                     Pakistan Sign Language
#> 844                pl                                     Polish
#> 845               pms                                Piedmontese
#> 846               pnb                            Western Punjabi
#> 847               pnt                                     Pontic
#> 848               pon                                  Pohnpeian
#> 849               pov                       Upper Guinea Crioulo
#> 850               ppl                                      Nawat
#> 851               ppu                              Papora-Hoanya
#> 852               pqm                     Maliseet-Passamaquoddy
#> 853               prg                                   Prussian
#> 854               pro                              Old Provençal
#> 855               prs                                       Dari
#> 856                ps                                     Pashto
#> 857             ps-af                       Pashto (Afghanistan)
#> 858             ps-pk                          Pashto (Pakistan)
#> 859               psh                           Southwest Pashai
#> 860               psi                           Southeast Pashai
#> 861               psu                          Sauraseni Prākrit
#> 862          psu-arab          Sauraseni Prākrit (Arabic script)
#> 863          psu-brah          Sauraseni Prākrit (Brahmi script)
#> 864          psu-deva      Sauraseni Prākrit (Devanagari script)
#> 865          psu-guru        Sauraseni Prākrit (Gurmukhi script)
#> 866                pt                                 Portuguese
#> 867         pt-ao1990   Portuguese (1990 Orthographic Agreement)
#> 868             pt-br                       Brazilian Portuguese
#> 869       pt-colb1945   Portuguese (1945 Orthographic Agreement)
#> 870             pt-pt                        European Portuguese
#> 871               pwn                                     Paiwan
#> 872               pyu                                     Puyuma
#> 873                qu                                    Quechua
#> 874               quc                                    Kʼicheʼ
#> 875               qug                Chimborazo Highland Quichua
#> 876               qwh                     Huaylas Ancash Quechua
#> 877               qxp                               Puno Quechua
#> 878               qxq                                    Qashqai
#> 879               qya                                     Quenya
#> 880               rag                                    Logooli
#> 881               rah                                      Rabha
#> 882               raj                                 Rajasthani
#> 883               rap                                    Rapanui
#> 884               rar                                 Rarotongan
#> 885               rcf                      Réunion Creole French
#> 886               rej                                     Rejang
#> 887               rgn                                   Romagnol
#> 888               rhg                                   Rohingya
#> 889          rhg-arab                   Rohingya (Arabic script)
#> 890          rhg-rohg          Rohingya (Hanifi Rohingya script)
#> 891               rif                                    Riffian
#> 892               rki                                  Arakanese
#> 893               rkt                                   Rangpuri
#> 894                rm                                    Romansh
#> 895          rm-puter                                      Putèr
#> 896          rm-rumgr                         Rumantsch Grischun
#> 897       rm-surmiran                                   Surmiran
#> 898        rm-sursilv                                  Sursilvan
#> 899        rm-sutsilv                                  Sutsilvan
#> 900       rm-vallader                                   Vallader
#> 901               rmc                          Carpathian Romani
#> 902               rmf                               Finnish Kalo
#> 903               rmg                        Traveller Norwegian
#> 904               rml                              Baltic Romani
#> 905          rml-cyrl            Baltic Romani (Cyrillic script)
#> 906               rmn                              Balkan Romani
#> 907               rmo                               Sinte Romani
#> 908               rmw                               Welsh-Romani
#> 909               rmy                                Vlax Romani
#> 910                rn                                      Rundi
#> 911                ro                                   Romanian
#> 912             ro-md                                  Moldavian
#> 913           roa-rup                                  Aromanian
#> 914          roa-tara                                  Tarantino
#> 915               rof                                      Rombo
#> 916               rom                                     Romany
#> 917               rsk                            Pannonian Rusyn
#> 918               rtm                                    Rotuman
#> 919                ru                                    Russian
#> 920       ru-petr1708              Russian (Petrine orthography)
#> 921               rue                                      Rusyn
#> 922               rug                                    Roviana
#> 923               ruo                             Istro Romanian
#> 924               rup                                  Aromanian
#> 925               ruq                           Megleno-Romanian
#> 926          ruq-cyrl         Megleno-Romanian (Cyrillic script)
#> 927          ruq-latn            Megleno-Romanian (Latin script)
#> 928               rut                                      Rutul
#> 929                rw                                Kinyarwanda
#> 930               rwk                                        Rwa
#> 931               rwr                            Marwari (India)
#> 932               rys                                    Yaeyama
#> 933          rys-hira                  Yaeyama (Hiragana script)
#> 934               ryu                                   Okinawan
#> 935          ryu-hira                 Okinawan (Hiragana script)
#> 936                sa                                   Sanskrit
#> 937           sa-sidd                  Sanskrit (Siddham script)
#> 938               sad                                    Sandawe
#> 939               sah                                      Yakut
#> 940               sam                          Samaritan Aramaic
#> 941               saq                                    Samburu
#> 942               sas                                      Sasak
#> 943               sat                                    Santali
#> 944          sat-beng                   Santali (Bengali script)
#> 945          sat-latn                     Santali (Latin script)
#> 946          sat-orya                     Santali (Oriya script)
#> 947               saz                                 Saurashtra
#> 948               sba                                    Ngambay
#> 949               sbp                                      Sangu
#> 950                sc                                  Sardinian
#> 951               scl                                      Shina
#> 952               scn                                   Sicilian
#> 953               sco                                      Scots
#> 954                sd                                     Sindhi
#> 955           sd-deva                 Sindhi (Devanagari script)
#> 956           sd-gujr                   Sindhi (Gujarati script)
#> 957           sd-khoj                     Sindhi (Khojki script)
#> 958           sd-sind                  Sindhi (Khudawadi script)
#> 959               sdc                        Sassarese Sardinian
#> 960               sdh                           Southern Kurdish
#> 961          sdh-arab           Southern Kurdish (Arabic script)
#> 962          sdh-latn            Southern Kurdish (Latin script)
#> 963                se                              Northern Sami
#> 964             se-fi                    Northern Sami (Finland)
#> 965             se-no                     Northern Sami (Norway)
#> 966             se-se                     Northern Sami (Sweden)
#> 967               see                                     Seneca
#> 968               seh                                       Sena
#> 969               sei                                       Seri
#> 970               sel                                     Selkup
#> 971               ser                                    Serrano
#> 972               ses                            Koyraboro Senni
#> 973                sg                                      Sango
#> 974               sga                                  Old Irish
#> 975               sgh                                    Shughni
#> 976               sgs                                 Samogitian
#> 977                sh                             Serbo-Croatian
#> 978           sh-cyrl           Serbo-Croatian (Cyrillic script)
#> 979           sh-latn              Serbo-Croatian (Latin script)
#> 980               shd                               Kundal Shahi
#> 981               shi                                  Tachelhit
#> 982          shi-latn                   Tachelhit (Latin script)
#> 983          shi-tfng                Tachelhit (Tifinagh script)
#> 984               shn                                       Shan
#> 985               shu                             Chadian Arabic
#> 986               shy                                    Shawiya
#> 987          shy-arab                    Shawiya (Arabic script)
#> 988          shy-latn                     Shawiya (Latin script)
#> 989          shy-tfng                  Shawiya (Tifinagh script)
#> 990                si                                    Sinhala
#> 991               sia                                Akkala Sami
#> 992               sid                                     Sidamo
#> 993            simple                             Simple English
#> 994               sjd                                Kildin Sami
#> 995               sje                                  Pite Sami
#> 996               sjk                                  Kemi Sami
#> 997               sjn                                   Sindarin
#> 998               sjo                                       Xibe
#> 999               sjt                                   Ter Sami
#> 1000              sju                                   Ume Sami
#> 1001               sk                                     Slovak
#> 1002              skr                                    Saraiki
#> 1003         skr-arab                    Saraiki (Arabic script)
#> 1004               sl                                  Slovenian
#> 1005              slh                       Southern Lushootseed
#> 1006              sli                             Lower Silesian
#> 1007              slr                                      Salar
#> 1008              sly                                    Selayar
#> 1009               sm                                     Samoan
#> 1010              sma                              Southern Sami
#> 1011              smj                                  Lule Sami
#> 1012              smn                                 Inari Sami
#> 1013              sms                                 Skolt Sami
#> 1014               sn                                      Shona
#> 1015              snk                                    Soninke
#> 1016               so                                     Somali
#> 1017              sog                                    Sogdien
#> 1018              spv                                 Sambalpuri
#> 1019               sq                                   Albanian
#> 1020               sr                                    Serbian
#> 1021          sr-cyrl                  Serbian (Cyrillic script)
#> 1022            sr-ec                  Serbian (Cyrillic script)
#> 1023            sr-el                     Serbian (Latin script)
#> 1024          sr-latn                     Serbian (Latin script)
#> 1025            sr-me                                Montenegrin
#> 1026              srn                               Sranan Tongo
#> 1027              sro                      Campidanese Sardinian
#> 1028              srq                                    Sirionó
#> 1029              srr                                      Serer
#> 1030               ss                                      Swati
#> 1031              ssf                                       Thao
#> 1032              ssy                                       Saho
#> 1033               st                             Southern Sotho
#> 1034              sth                                     Shelta
#> 1035              stq                          Saterland Frisian
#> 1036              str                             Straits Salish
#> 1037              sty                             Siberian Tatar
#> 1038               su                                  Sundanese
#> 1039              suk                                     Sukuma
#> 1040              sus                                       Susu
#> 1041              sux                                   Sumerian
#> 1042         sux-latn                    Sumerian (Latin script)
#> 1043         sux-xsux                Sumerian (Cuneiform script)
#> 1044               sv                                    Swedish
#> 1045              sva                                       Svan
#> 1046               sw                                    Swahili
#> 1047            sw-cd                              Congo Swahili
#> 1048              swb                                   Comorian
#> 1049              sxr                                     Saaroa
#> 1050              sxu                                Upper Saxon
#> 1051              syc                           Classical Syriac
#> 1052              syl                                    Sylheti
#> 1053         syl-beng                   Sylheti (Bengali script)
#> 1054              syr                                     Syriac
#> 1055              szl                                   Silesian
#> 1056              szy                                   Sakizaya
#> 1057               ta                                      Tamil
#> 1058              tao                                       Yami
#> 1059              tay                                     Atayal
#> 1060              tbl                                      Tboli
#> 1061              tce                          Southern Tutchone
#> 1062              tcy                                       Tulu
#> 1063              tdd                                   Tai Nuea
#> 1064               te                                     Telugu
#> 1065              tem                                      Timne
#> 1066              teo                                       Teso
#> 1067              ter                                     Tereno
#> 1068              tet                                      Tetum
#> 1069               tg                                      Tajik
#> 1070          tg-cyrl                    Tajik (Cyrillic script)
#> 1071          tg-latn                       Tajik (Latin script)
#> 1072              tgx                                     Tagish
#> 1073               th                                       Thai
#> 1074              thq                              Kochila Tharu
#> 1075              thr                                 Rana Tharu
#> 1076              tht                                    Tahltan
#> 1077               ti                                   Tigrinya
#> 1078              tig                                      Tigre
#> 1079              tiv                                        Tiv
#> 1080              tji                             Northern Tujia
#> 1081               tk                                    Turkmen
#> 1082              tkl                                    Tokelau
#> 1083              tkr                                    Tsakhur
#> 1084               tl                                    Tagalog
#> 1085              tlb                                     Tobelo
#> 1086              tlh                                    Klingon
#> 1087         tlh-latn                     Klingon (Latin script)
#> 1088         tlh-piqd                   Klingon (Klingon script)
#> 1089              tli                                    Tlingit
#> 1090              tly                                     Talysh
#> 1091         tly-cyrl                   Talysh (Cyrillic script)
#> 1092              tmh                                   Tamashek
#> 1093               tn                                     Tswana
#> 1094              tnq                                      Taíno
#> 1095               to                                     Tongan
#> 1096              tog                                Nyasa Tonga
#> 1097              toi                            Tonga (Botatwe)
#> 1098              tok                                  Toki Pona
#> 1099              tpi                                  Tok Pisin
#> 1100               tr                                    Turkish
#> 1101              tru                                     Turoyo
#> 1102              trv                                     Taroko
#> 1103              trw                                    Torwali
#> 1104               ts                                     Tsonga
#> 1105              tsd                                  Tsakonian
#> 1106              tsi                                  Tsimshian
#> 1107              tsu                                      Tsou 
#> 1108               tt                                      Tatar
#> 1109          tt-cyrl                    Tatar (Cyrillic script)
#> 1110          tt-latn                       Tatar (Latin script)
#> 1111              ttj                                      Tooro
#> 1112              ttm                          Northern Tutchone
#> 1113              ttt                                 Muslim Tat
#> 1114              tui                                     Tupuri
#> 1115              tum                                    Tumbuka
#> 1116              tvl                                     Tuvalu
#> 1117              tvu                                      Tunen
#> 1118               tw                                        Twi
#> 1119              twq                                    Tasawaq
#> 1120              txg                                     Tangut
#> 1121         txo-beng                      Toto (Bengali script)
#> 1122         txo-toto                         Toto (Toto script)
#> 1123               ty                                   Tahitian
#> 1124              tyv                                   Tuvinian
#> 1125              tzl                                   Talossan
#> 1126              tzm                    Central Atlas Tamazight
#> 1127              udm                                     Udmurt
#> 1128               ug                                     Uyghur
#> 1129          ug-arab                     Uyghur (Arabic script)
#> 1130          ug-latn                      Uyghur (Latin script)
#> 1131              uga                                   Ugaritic
#> 1132               uk                                  Ukrainian
#> 1133              uln                               Unserdeutsch
#> 1134              umb                                    Umbundu
#> 1135              umu                                     Munsee
#> 1136              und                      undetermined language
#> 1137              unr                                    Mundari
#> 1138         unr-deva                Mundari (Devanagari script)
#> 1139         unr-nagm               Mundari (Nag Mundari script)
#> 1140               ur                                       Urdu
#> 1141              uun                                      Pazeh
#> 1142               uz                                      Uzbek
#> 1143          uz-cyrl                    Uzbek (Cyrillic script)
#> 1144          uz-latn                       Uzbek (Latin script)
#> 1145              vai                                        Vai
#> 1146               ve                                      Venda
#> 1147              vec                                   Venetian
#> 1148              vep                                       Veps
#> 1149               vi                                 Vietnamese
#> 1150          vi-hani                    Vietnamese (Han script)
#> 1151              vls                               West Flemish
#> 1152              vmf                            Main-Franconian
#> 1153              vmw                                    Makhuwa
#> 1154               vo                                    Volapük
#> 1155              vot                                      Votic
#> 1156              vro                                       Võro
#> 1157              vun                                      Vunjo
#> 1158              vut                                       Vute
#> 1159               wa                                    Walloon
#> 1160              wae                                     Walser
#> 1161              wal                                   Wolaytta
#> 1162              war                                      Waray
#> 1163              was                                      Washo
#> 1164              wbp                                   Warlpiri
#> 1165              wes                          Pidgin (Cameroon)
#> 1166              wlm                               Middle Welsh
#> 1167              wls                                  Wallisian
#> 1168              wlx                                       Wali
#> 1169               wo                                      Wolof
#> 1170              wsg                             Adilabad Gondi
#> 1171              wsv                        Wotapuri-Katarqalai
#> 1172              wuu                                         Wu
#> 1173         wuu-hans                 Wu (Simplified Han script)
#> 1174         wuu-hant                Wu (Traditional Han script)
#> 1175              wya                                    Wyandot
#> 1176              wyi                                 Woiwurrung
#> 1177              xal                                     Kalmyk
#> 1178              xbm                              Middle Breton
#> 1179               xh                                      Xhosa
#> 1180              xmf                                 Mingrelian
#> 1181              xnb                                 Kanakanavu
#> 1182              xno                               Anglo-Norman
#> 1183              xnr                                     Kangri
#> 1184         xnr-deva                 Kangri (Devanagari script)
#> 1185         xnr-takr                      Kangri (Takri script)
#> 1186              xog                                       Soga
#> 1187              xon                                   Konkomba
#> 1188              xpu                                      Punic
#> 1189              xsu                                     Sanumá
#> 1190              xsy                                   Saisiyat
#> 1191              yao                                        Yao
#> 1192              yap                                     Yapese
#> 1193              yas                                     Nugunu
#> 1194              yat                                    Yambeta
#> 1195              yav                                    Yangben
#> 1196              ybb                                      Yemba
#> 1197              yec                                    Yeniche
#> 1198               yi                                    Yiddish
#> 1199              ykg                            Tundra Yukaghir
#> 1200               yo                                     Yoruba
#> 1201              yoi                                   Yonaguni
#> 1202         yoi-hira                 Yonaguni (Hiragana script)
#> 1203              yox                                      Yoron
#> 1204         yox-hira                    Yoron (Hiragana script)
#> 1205              yrk                                     Nenets
#> 1206              yrl                                  Nheengatu
#> 1207              yua                               Yucatec Maya
#> 1208              yue                                  Cantonese
#> 1209         yue-hans          Cantonese (Simplified Han script)
#> 1210         yue-hant         Cantonese (Traditional Han script)
#> 1211               za                                     Zhuang
#> 1212              zai                            Isthmus Zapotec
#> 1213              zap                                    Zapotec
#> 1214              zbl                                Blissymbols
#> 1215              zea                                  Zeelandic
#> 1216              zen                                     Zenaga
#> 1217              zgh                Standard Moroccan Tamazight
#> 1218         zgh-latn Standard Moroccan Tamazight (Latin script)
#> 1219               zh                                    Chinese
#> 1220     zh-classical                           Literary Chinese
#> 1221            zh-cn                            Chinese (China)
#> 1222          zh-hans                         Simplified Chinese
#> 1223          zh-hant                        Traditional Chinese
#> 1224            zh-hk                        Chinese (Hong Kong)
#> 1225       zh-min-nan                                     Minnan
#> 1226            zh-mo                            Chinese (Macau)
#> 1227            zh-my                         Chinese (Malaysia)
#> 1228            zh-sg                        Chinese (Singapore)
#> 1229            zh-tw                           Chinese (Taiwan)
#> 1230           zh-yue                                  Cantonese
#> 1231              zmi                      Negeri Sembilan Malay
#> 1232              zpu                            Yalálag Zapotec
#> 1233               zu                                       Zulu
#> 1234              zun                                       Zuni
#> 1235              zxx                      no linguistic content
#> 1236              zza                                       Zaza
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
#> 18                     تونسي / Tûnsî
#> 19                             تونسي
#> 20                             Tûnsî
#> 21                                  
#> 22                                  
#> 23                                  
#> 24                         Afrikaans
#> 25                                  
#> 26                                  
#> 27                                  
#> 28          Aanteegan an' Baabyuudan
#> 29                                  
#> 30                                  
#> 31                                  
#> 32                                  
#> 33                                  
#> 34                                  
#> 35                                  
#> 36                                  
#> 37                                  
#> 38                                  
#> 39                                  
#> 40                                  
#> 41                              Gegë
#> 42                                  
#> 43                       Alemannisch
#> 44                         алтай тил
#> 45                                  
#> 46                              አማርኛ
#> 47                           Pangcah
#> 48                                  
#> 49                          aragonés
#> 50                                  
#> 51                           Ænglisc
#> 52                             Obolo
#> 53                             अंगिका
#> 54                              شامي
#> 55                                  
#> 56                           العربية
#> 57                                  
#> 58                             ܐܪܡܝܐ
#> 59                                  
#> 60                        mapudungun
#> 61                                  
#> 62                                  
#> 63                          جازايرية
#> 64                                  
#> 65                                  
#> 66                           الدارجة
#> 67                                  
#> 68                                  
#> 69                              مصرى
#> 70                            অসমীয়া
#> 71                                  
#> 72            American sign language
#> 73                         asturianu
#> 74                         Atikamekw
#> 75                                  
#> 76                              авар
#> 77                            Kotava
#> 78                              अवधी
#> 79                                  
#> 80                                  
#> 81                         Aymar aru
#> 82                                  
#> 83                      azərbaycanca
#> 84                                  
#> 85                            تۆرکجه
#> 86                         башҡортса
#> 87                                  
#> 88                                  
#> 89                                  
#> 90                         Basa Bali
#> 91                              ᬩᬲᬩᬮᬶ
#> 92                          Boarisch
#> 93                                  
#> 94                        žemaitėška
#> 95                                  
#> 96                        Batak Toba
#> 97                        Batak Toba
#> 98                                  
#> 99                      جهلسری بلوچی
#> 100                            wawle
#> 101                    Bikol Central
#> 102                       Bajau Sama
#> 103                       беларуская
#> 104         беларуская (тарашкевіца)
#> 105         беларуская (тарашкевіца)
#> 106                                 
#> 107                                 
#> 108                           Betawi
#> 109                                 
#> 110                                 
#> 111                                 
#> 112                                 
#> 113                                 
#> 114                                 
#> 115                                 
#> 116                                 
#> 117                                 
#> 118                                 
#> 119                                 
#> 120                        български
#> 121                         हरियाणवी
#> 122                                 
#> 123                                 
#> 124                  روچ کپتین بلوچی
#> 125                                 
#> 126                                 
#> 127                                 
#> 128                                 
#> 129                           भोजपुरी
#> 130                                 
#> 131                                 
#> 132                                 
#> 133                                 
#> 134                           भोजपुरी
#> 135                          Bislama
#> 136                                 
#> 137                                 
#> 138                           Banjar
#> 139                                 
#> 140                                 
#> 141                                 
#> 142                                 
#> 143                                 
#> 144                       ပအိုဝ်ႏဘာႏသာႏ
#> 145                                 
#> 146                                 
#> 147                       bamanankan
#> 148                            বাংলা
#> 149                                 
#> 150                            བོད་ཡིག
#> 151                        bòo pìkkà
#> 152                                 
#> 153                 বিষ্ণুপ্রিয়া মণিপুরী
#> 154                          بختیاری
#> 155                                 
#> 156                        brezhoneg
#> 157                                 
#> 158                           Bráhuí
#> 159                                 
#> 160                                 
#> 161                         bosanski
#> 162                                 
#> 163                                 
#> 164                                 
#> 165                                 
#> 166                 Batak Mandailing
#> 167                   Iriga Bicolano
#> 168                                 
#> 169                                 
#> 170                                 
#> 171                                 
#> 172                         Basa Ugi
#> 173                            ᨅᨔ ᨕᨘᨁᨗ
#> 174                                 
#> 175                                 
#> 176                                 
#> 177                           буряад
#> 178                                 
#> 179                                 
#> 180                                 
#> 181                                 
#> 182                           català
#> 183                                 
#> 184                                 
#> 185                                 
#> 186                                 
#> 187                                 
#> 188           Chavacano de Zamboanga
#> 189                                 
#> 190                             𑄌𑄋𑄴𑄟𑄳𑄦
#> 191                                 
#> 192           閩東語 / Mìng-dĕ̤ng-ngṳ̄
#> 193                                 
#> 194               閩東語（傳統漢字）
#> 195       Mìng-dĕ̤ng-ngṳ̄ (Bàng-uâ-cê)
#> 196                                 
#> 197                          нохчийн
#> 198                          Cebuano
#> 199                                 
#> 200                          Chamoru
#> 201                                 
#> 202                                 
#> 203                                 
#> 204                                 
#> 205                      chinuk wawa
#> 206                    Chahta anumpa
#> 207                                 
#> 208                              ᏣᎳᎩ
#> 209                  Tsetsêhestâhese
#> 210                                 
#> 211                                 
#> 212                                 
#> 213                                 
#> 214                                 
#> 215                                 
#> 216                                 
#> 217                                 
#> 218                                 
#> 219                                 
#> 220                                 
#> 221                                 
#> 222                            کوردی
#> 223                                 
#> 224                                 
#> 225                                 
#> 226                                 
#> 227                                 
#> 228                                 
#> 229                                 
#> 230                                 
#> 231                                 
#> 232                                 
#> 233                                 
#> 234                                 
#> 235                            corsu
#> 236                     ϯⲙⲉⲧⲣⲉⲙⲛ̀ⲭⲏⲙⲓ
#> 237                         Capiceño
#> 238              莆仙語 / Pó-sing-gṳ̂
#> 239                   莆仙语（简体）
#> 240                   莆仙語（繁體）
#> 241           Pó-sing-gṳ̂ (Báⁿ-uā-ci̍)
#> 242            Nēhiyawēwin / ᓀᐦᐃᔭᐍᐏᐣ
#> 243                                 
#> 244                                 
#> 245                     qırımtatarca
#> 246          къырымтатарджа (Кирилл)
#> 247             qırımtatarca (Latin)
#> 248                          tatarşa
#> 249                                 
#> 250                                 
#> 251                                 
#> 252                                 
#> 253                                 
#> 254                                 
#> 255                          čeština
#> 256                       kaszëbsczi
#> 257                                 
#> 258                                 
#> 259          словѣньскъ / ⰔⰎⰑⰂⰡⰐⰠⰔⰍⰟ
#> 260                          чӑвашла
#> 261                          Cymraeg
#> 262                            dansk
#> 263                         dagbanli
#> 264                                 
#> 265                                 
#> 266                                 
#> 267                                 
#> 268                          Deutsch
#> 269                                 
#> 270         Österreichisches Deutsch
#> 271            Schweizer Hochdeutsch
#> 272               Deutsch (Sie-Form)
#> 273                                 
#> 274                                 
#> 275                          Dagaare
#> 276                                 
#> 277                         Thuɔŋjäŋ
#> 278                           Zazaki
#> 279                                 
#> 280                      долган тыла
#> 281                                 
#> 282                                 
#> 283                                 
#> 284                                 
#> 285                                 
#> 286                     dolnoserbski
#> 287                                 
#> 288                     Kadazandusun
#> 289                            डोटेली
#> 290                            Duálá
#> 291                                 
#> 292                                 
#> 293                            ދިވެހިބަސް
#> 294                                 
#> 295                                 
#> 296                             ཇོང་ཁ
#> 297                                 
#> 298                                 
#> 299                           eʋegbe
#> 300                             Efịk
#> 301               emiliàn e rumagnòl
#> 302                                 
#> 303                                 
#> 304                                 
#> 305                         Ελληνικά
#> 306                                 
#> 307                                 
#> 308                                 
#> 309               emiliàn e rumagnòl
#> 310                          English
#> 311                                 
#> 312                 Canadian English
#> 313                  British English
#> 314                                 
#> 315                                 
#> 316                                 
#> 317                                 
#> 318                                 
#> 319                        Esperanto
#> 320                                 
#> 321                                 
#> 322                          español
#> 323        español de América Latina
#> 324                                 
#> 325                 español (formal)
#> 326                                 
#> 327                                 
#> 328                                 
#> 329                            eesti
#> 330                                 
#> 331                                 
#> 332                                 
#> 333                          euskara
#> 334                                 
#> 335                        estremeñu
#> 336                                 
#> 337                            فارسی
#> 338                                 
#> 339                                 
#> 340                          mfantse
#> 341                         Fulfulde
#> 342                            suomi
#> 343                                 
#> 344                        meänkieli
#> 345                             võro
#> 346                 Na Vosa Vakaviti
#> 347                                 
#> 348                                 
#> 349                         føroyskt
#> 350                           fɔ̀ngbè
#> 351                                 
#> 352                         français
#> 353                                 
#> 354                                 
#> 355                  français cadien
#> 356                                 
#> 357                                 
#> 358                                 
#> 359                          arpetan
#> 360                       Nordfriisk
#> 361                                 
#> 362                                 
#> 363                                 
#> 364                                 
#> 365                           furlan
#> 366                   poor’íŋ belé’ŋ
#> 367                            Frysk
#> 368                          Gaeilge
#> 369                               Ga
#> 370                           Gagauz
#> 371                             贛語
#> 372                     赣语（简体）
#> 373                     贛語（繁體）
#> 374                                 
#> 375                                 
#> 376                                 
#> 377                                 
#> 378                                 
#> 379                                 
#> 380                                 
#> 381                                 
#> 382                  kréyòl Gwadloup
#> 383                 kriyòl gwiyannen
#> 384                         Gàidhlig
#> 385                                 
#> 386                                 
#> 387                                 
#> 388                                 
#> 389                                 
#> 390                           galego
#> 391                             на̄ни
#> 392                                 
#> 393                            گیلکی
#> 394                                 
#> 395                                 
#> 396                                 
#> 397                          Avañe'ẽ
#> 398                                 
#> 399     गोंयची कोंकणी / Gõychi Konknni
#> 400                      गोंयची कोंकणी
#> 401                   Gõychi Konknni
#> 402                                 
#> 403                 Bahasa Hulontalo
#> 404                           𐌲𐌿𐍄𐌹𐍃𐌺
#> 405                  Ghanaian Pidgin
#> 406                                 
#> 407                  Ἀρχαία ἑλληνικὴ
#> 408                                 
#> 409                      Alemannisch
#> 410                                 
#> 411                           ગુજરાતી
#> 412                       wayuunaiki
#> 413                         farefare
#> 414                           gungbe
#> 415                                 
#> 416                            Gaelg
#> 417                                 
#> 418                                 
#> 419                            Hausa
#> 420                                 
#> 421                                 
#> 422                                 
#> 423                                 
#> 424              客家語 / Hak-kâ-ngî
#> 425                   客家语（简体）
#> 426                   客家語（繁體）
#> 427          Hak-kâ-ngî (Pha̍k-fa-sṳ)
#> 428                                 
#> 429                          Hawaiʻi
#> 430                                 
#> 431                                 
#> 432                                 
#> 433                            עברית
#> 434                            हिन्दी
#> 435                                 
#> 436                                 
#> 437                       Fiji Hindi
#> 438                                 
#> 439                       Fiji Hindi
#> 440                          Ilonggo
#> 441                                 
#> 442                                 
#> 443                                 
#> 444                          kihunde
#> 445                                 
#> 446                                 
#> 447                                 
#> 448                            ہندکو
#> 449                        Hiri Motu
#> 450                                 
#> 451                               Ho
#> 452                         hrvatski
#> 453                          Hunsrik
#> 454                    hornjoserbsce
#> 455                             湘語
#> 456                                 
#> 457                                 
#> 458                   Kreyòl ayisyen
#> 459                                 
#> 460                           magyar
#> 461                  magyar (formal)
#> 462                                 
#> 463                                 
#> 464                          հայերեն
#> 465                   Արեւմտահայերէն
#> 466                       Otsiherero
#> 467                      interlingua
#> 468                        Jaku Iban
#> 469                           ibibio
#> 470                 Bahasa Indonesia
#> 471                      Interlingue
#> 472                             Igbo
#> 473                                 
#> 474                            Igala
#> 475                             ꆇꉙ
#> 476                        Iñupiatun
#> 477                           ᐃᓄᒃᑎᑐᑦ
#> 478                        inuktitut
#> 479                                 
#> 480                          Ilokano
#> 481                         гӀалгӀай
#> 482                              Ido
#> 483                         íslenska
#> 484                                 
#> 485                                 
#> 486                  меджусловјанскы
#> 487                  medžuslovjansky
#> 488                         italiano
#> 489               ᐃᓄᒃᑎᑐᑦ / inuktitut
#> 490                                 
#> 491                           日本語
#> 492                                 
#> 493                                 
#> 494                                 
#> 495                                 
#> 496                                 
#> 497                           Patois
#> 498                      la .lojban.
#> 499                                 
#> 500                                 
#> 501                                 
#> 502                                 
#> 503                                 
#> 504                                 
#> 505                             jysk
#> 506                             Jawa
#> 507                               ꦗꦮ
#> 508                          ქართული
#> 509                    Qaraqalpaqsha
#> 510                        Taqbaylit
#> 511                                 
#> 512                      Karai-karai
#> 513                              Jju
#> 514                                 
#> 515                                 
#> 516                         адыгэбзэ
#> 517                         адыгэбзэ
#> 518                                 
#> 519                           Kabɩyɛ
#> 520                             Tyap
#> 521                                 
#> 522                                 
#> 523                     kabuverdianu
#> 524                                 
#> 525                                 
#> 526                                 
#> 527                                 
#> 528                            Kongo
#> 529                         Kumoring
#> 530                                 
#> 531                                 
#> 532                                 
#> 533                                 
#> 534                                 
#> 535                                 
#> 536                            کھوار
#> 537                           Gĩkũyũ
#> 538                                 
#> 539                        Kırmancki
#> 540                         Kwanyama
#> 541                            хакас
#> 542                              ဖၠုံလိက်
#> 543                          қазақша
#> 544                  قازاقشا (تٴوتە)
#> 545                  قازاقشا (جۇنگو)
#> 546                  қазақша (кирил)
#> 547              қазақша (Қазақстан)
#> 548                  qazaqşa (latın)
#> 549                qazaqşa (Türkïya)
#> 550                                 
#> 551                      kalaallisut
#> 552                                 
#> 553                                 
#> 554                                 
#> 555                                 
#> 556                                 
#> 557                         ភាសាខ្មែរ
#> 558                                 
#> 559                                 
#> 560                                 
#> 561                                 
#> 562                                 
#> 563                             ಕನ್ನಡ
#> 564                     Yerwa Kanuri
#> 565                                 
#> 566                                 
#> 567                           한국어
#> 568                                 
#> 569                                 
#> 570                                 
#> 571                           조선말
#> 572                       перем коми
#> 573                                 
#> 574                                 
#> 575                                 
#> 576                                 
#> 577                           kanuri
#> 578                 къарачай-малкъар
#> 579                             Krio
#> 580                        Kinaray-a
#> 581                           karjal
#> 582                                 
#> 583                             کٲشُر
#> 584                             کٲشُر
#> 585                             कॉशुर
#> 586                                 
#> 587                                 
#> 588                       Ripoarisch
#> 589                               စှီၤ
#> 590                                 
#> 591                            kurdî
#> 592                   کوردی (عەرەبی)
#> 593                   kurdî (latînî)
#> 594                          къумукъ
#> 595                           Kʋsaal
#> 596                                 
#> 597                             коми
#> 598                         kernowek
#> 599                                 
#> 600                                 
#> 601                         кыргызча
#> 602                                 
#> 603                                 
#> 604                           Latina
#> 605                           Ladino
#> 606                                 
#> 607                                 
#> 608                                 
#> 609                                 
#> 610                                 
#> 611                   Lëtzebuergesch
#> 612                            лакку
#> 613                                 
#> 614                                 
#> 615                                 
#> 616                            лезги
#> 617               Lingua Franca Nova
#> 618                          Luganda
#> 619                         Limburgs
#> 620                           Ligure
#> 621                                 
#> 622                                 
#> 623                         Līvõ kēļ
#> 624                      Lampung Api
#> 625                             لەکی
#> 626                                 
#> 627                            Ladin
#> 628                                 
#> 629                          lombard
#> 630                          lingála
#> 631                                 
#> 632                              ລາວ
#> 633                                 
#> 634                                 
#> 635                           Silozi
#> 636                      لۊری شومالی
#> 637                                 
#> 638                         lietuvių
#> 639                          latgaļu
#> 640                                 
#> 641                           ciluba
#> 642                                 
#> 643                                 
#> 644                                 
#> 645                       Mizo ţawng
#> 646                                 
#> 647                      لئری دوٙمینی
#> 648                         latviešu
#> 649                             文言
#> 650                           Lazuri
#> 651                          Madhurâ
#> 652                                 
#> 653                             मगही
#> 654                            मैथिली
#> 655                                 
#> 656                                 
#> 657                                 
#> 658                  Basa Banyumasan
#> 659                                 
#> 660                                 
#> 661                                 
#> 662                                 
#> 663                                 
#> 664                          мокшень
#> 665                                 
#> 666                                 
#> 667                                 
#> 668                                 
#> 669                                 
#> 670                                 
#> 671                                 
#> 672                         Malagasy
#> 673                                 
#> 674                                 
#> 675                                 
#> 676                             Ebon
#> 677                                 
#> 678                       олык марий
#> 679                            Māori
#> 680                                 
#> 681                                 
#> 682                      Minangkabau
#> 683                                 
#> 684                                 
#> 685                                 
#> 686                       македонски
#> 687                           മലയാളം
#> 688                           монгол
#> 689                                 
#> 690                      manju gisun
#> 691                      manju gisun
#> 692                      ᠮᠠᠨᠵᡠ ᡤᡳᠰᡠᠨ
#> 693                         ꯃꯤꯇꯩ ꯂꯣꯟ
#> 694                                 
#> 695                           ဘာသာမန်
#> 696                     молдовеняскэ
#> 697                                 
#> 698                                 
#> 699                            moore
#> 700                            मराठी
#> 701                             Mara
#> 702                       кырык мары
#> 703                                 
#> 704                                 
#> 705                    Bahasa Melayu
#> 706                       بهاس ملايو
#> 707                                 
#> 708                            Malti
#> 709                                 
#> 710                   Baso Palembang
#> 711                                 
#> 712                          Mvskoke
#> 713                                 
#> 714                                 
#> 715                                 
#> 716                         Mirandés
#> 717                                 
#> 718                                 
#> 719                                 
#> 720                                 
#> 721                        မြန်မာဘာသာ
#> 722                                 
#> 723                           эрзянь
#> 724                          مازِرونی
#> 725                   Dorerin Naoero
#> 726                          Nāhuatl
#> 727              閩南語 / Bân-lâm-gí
#> 728                                 
#> 729                                 
#> 730               閩南語（傳統漢字）
#> 731           Bân-lâm-gí (Pe̍h-ōe-jī)
#> 732              Bân-lâm-gí (Tâi-lô)
#> 733                       Napulitano
#> 734                                 
#> 735                     norsk bokmål
#> 736                                 
#> 737                     Plattdüütsch
#> 738                     Nedersaksies
#> 739                            नेपाली
#> 740                        नेपाल भाषा
#> 741                        Oshiwambo
#> 742                                 
#> 743                          Li Niha
#> 744                              కొలామి
#> 745                             Niuē
#> 746                                 
#> 747                       Nederlands
#> 748                                 
#> 749           Nederlands (informeel)
#> 750                                 
#> 751                                 
#> 752                            nawdm
#> 753                    norsk nynorsk
#> 754                                 
#> 755                                 
#> 756                                 
#> 757                            norsk
#> 758                            ᨣᩤᩴᨾᩮᩬᩥᨦ
#> 759                                 
#> 760                          ногайша
#> 761                                 
#> 762                                 
#> 763                           Novial
#> 764                              ߒߞߏ
#> 765              isiNdebele seSewula
#> 766                                 
#> 767                                 
#> 768                        Nouormand
#> 769                                 
#> 770                 Sesotho sa Leboa
#> 771                             Nupe
#> 772                                 
#> 773                      Diné bizaad
#> 774                                 
#> 775                                 
#> 776                        Chi-Chewa
#> 777                                 
#> 778                       runyankore
#> 779                         Orunyoro
#> 780                           Nyunga
#> 781                                 
#> 782                                 
#> 783                          occitan
#> 784                                 
#> 785                                 
#> 786                                 
#> 787                                 
#> 788                      Ojibwemowin
#> 789                                 
#> 790                                 
#> 791                                 
#> 792                                 
#> 793                                 
#> 794                                 
#> 795                                 
#> 796                    livvinkarjala
#> 797                           Oromoo
#> 798                                 
#> 799                                 
#> 800                              ଓଡ଼ିଆ
#> 801                             ирон
#> 802                                 
#> 803                                 
#> 804                                 
#> 805                                 
#> 806                                 
#> 807                                 
#> 808                                 
#> 809                                 
#> 810                            ਪੰਜਾਬੀ
#> 811                       Pangasinan
#> 812                                 
#> 813                                 
#> 814                                 
#> 815                                 
#> 816                      Kapampangan
#> 817                                 
#> 818                       Papiamentu
#> 819               Papiamento (Aruba)
#> 820                                 
#> 821                                 
#> 822                                 
#> 823                           Picard
#> 824                            Naijá
#> 825                          Deitsch
#> 826                     Plautdietsch
#> 827                                 
#> 828                         Pälzisch
#> 829                                 
#> 830                                 
#> 831                                 
#> 832                                 
#> 833                                 
#> 834                                 
#> 835                                 
#> 836                                 
#> 837                                 
#> 838                             पालि
#> 839                                 
#> 840                 Norfuk / Pitkern
#> 841                                 
#> 842                                 
#> 843                                 
#> 844                           polski
#> 845                       Piemontèis
#> 846                           پنجابی
#> 847                         Ποντιακά
#> 848                                 
#> 849                                 
#> 850                            Nawat
#> 851                                 
#> 852                                 
#> 853                        prūsiskan
#> 854                                 
#> 855                                 
#> 856                             پښتو
#> 857                                 
#> 858                                 
#> 859                                 
#> 860                                 
#> 861                                 
#> 862                                 
#> 863                                 
#> 864                                 
#> 865                                 
#> 866                        português
#> 867                                 
#> 868              português do Brasil
#> 869                                 
#> 870                                 
#> 871                       pinayuanan
#> 872                                 
#> 873                        Runa Simi
#> 874                                 
#> 875                       Runa shimi
#> 876                                 
#> 877                                 
#> 878                                 
#> 879                                 
#> 880                                 
#> 881                                 
#> 882                                 
#> 883                                 
#> 884                                 
#> 885                                 
#> 886                                 
#> 887                         Rumagnôl
#> 888                                 
#> 889                                 
#> 890                                 
#> 891                          Tarifit
#> 892                              ရခိုင်
#> 893                                 
#> 894                        rumantsch
#> 895                                 
#> 896                                 
#> 897                                 
#> 898                                 
#> 899                                 
#> 900                                 
#> 901                      romaňi čhib
#> 902                                 
#> 903                                 
#> 904                                 
#> 905                                 
#> 906                                 
#> 907                                 
#> 908                                 
#> 909                      romani čhib
#> 910                         ikirundi
#> 911                           română
#> 912                                 
#> 913                      armãneashti
#> 914                        tarandíne
#> 915                                 
#> 916                                 
#> 917                            руски
#> 918                                 
#> 919                          русский
#> 920                                 
#> 921                       русиньскый
#> 922                                 
#> 923                                 
#> 924                      armãneashti
#> 925                         Vlăheşte
#> 926                         Влахесте
#> 927                         Vlăheşte
#> 928                       мыхаӀбишды
#> 929                     Ikinyarwanda
#> 930                                 
#> 931                                 
#> 932                                 
#> 933                                 
#> 934                     うちなーぐち
#> 935                                 
#> 936                            संस्कृतम्
#> 937                                 
#> 938                                 
#> 939                        саха тыла
#> 940                                 
#> 941                                 
#> 942                            Sasak
#> 943                          ᱥᱟᱱᱛᱟᱲᱤ
#> 944                                 
#> 945                                 
#> 946                                 
#> 947                                 
#> 948                                 
#> 949                                 
#> 950                            sardu
#> 951                                 
#> 952                        sicilianu
#> 953                            Scots
#> 954                             سنڌي
#> 955                                 
#> 956                                 
#> 957                                 
#> 958                                 
#> 959                        Sassaresu
#> 960                      کوردی خوارگ
#> 961                                 
#> 962                                 
#> 963                  davvisámegiella
#> 964   davvisámegiella (Suoma bealde)
#> 965  davvisámegiella (Norgga bealde)
#> 966   davvisámegiella (Ruoŧa bealde)
#> 967                                 
#> 968                                 
#> 969                      Cmique Itom
#> 970                                 
#> 971                                 
#> 972                  Koyraboro Senni
#> 973                            Sängö
#> 974                                 
#> 975                                 
#> 976                       žemaitėška
#> 977  srpskohrvatski / српскохрватски
#> 978        српскохрватски (ћирилица)
#> 979        srpskohrvatski (latinica)
#> 980                                 
#> 981                          Taclḥit
#> 982                          Taclḥit
#> 983                          ⵜⴰⵛⵍⵃⵉⵜ
#> 984                               တႆး
#> 985                                 
#> 986                          tacawit
#> 987                                 
#> 988                          tacawit
#> 989                                 
#> 990                             සිංහල
#> 991                                 
#> 992                                 
#> 993                   Simple English
#> 994                  кӣллт са̄мь кӣлл
#> 995                  bidumsámegiella
#> 996                                 
#> 997                                 
#> 998                                 
#> 999                                 
#> 1000                                
#> 1001                      slovenčina
#> 1002                         سرائیکی
#> 1003                         سرائیکی
#> 1004                     slovenščina
#> 1005                                
#> 1006                        Schläsch
#> 1007                                
#> 1008                                
#> 1009                    Gagana Samoa
#> 1010                   åarjelsaemien
#> 1011                                
#> 1012                     anarâškielâ
#> 1013                nuõrttsääʹmǩiõll
#> 1014                        chiShona
#> 1015                                
#> 1016                      Soomaaliga
#> 1017                                
#> 1018                                
#> 1019                           shqip
#> 1020                 српски / srpski
#> 1021               српски (ћирилица)
#> 1022               српски (ћирилица)
#> 1023               srpski (latinica)
#> 1024               srpski (latinica)
#> 1025                                
#> 1026                     Sranantongo
#> 1027               sardu campidanesu
#> 1028                                
#> 1029                                
#> 1030                         SiSwati
#> 1031                                
#> 1032                                
#> 1033                         Sesotho
#> 1034                                
#> 1035                       Seeltersk
#> 1036                                
#> 1037                      себертатар
#> 1038                           Sunda
#> 1039                                
#> 1040                                
#> 1041                                
#> 1042                                
#> 1043                                
#> 1044                         svenska
#> 1045                                
#> 1046                       Kiswahili
#> 1047                                
#> 1048                                
#> 1049                                
#> 1050                                
#> 1051                                
#> 1052                           ꠍꠤꠟꠐꠤ
#> 1053                                
#> 1054                                
#> 1055                         ślůnski
#> 1056                        Sakizaya
#> 1057                            தமிழ்
#> 1058                                
#> 1059                           Tayal
#> 1060                                
#> 1061                                
#> 1062                            ತುಳು
#> 1063                    ᥖᥭᥰ ᥖᥬᥲ ᥑᥨᥒᥰ
#> 1064                           తెలుగు
#> 1065                                
#> 1066                                
#> 1067                                
#> 1068                           tetun
#> 1069                          тоҷикӣ
#> 1070                          тоҷикӣ
#> 1071                          tojikī
#> 1072                                
#> 1073                             ไทย
#> 1074                                
#> 1075                                
#> 1076                                
#> 1077                            ትግርኛ
#> 1078                             ትግሬ
#> 1079                                
#> 1080                                
#> 1081                       Türkmençe
#> 1082                                
#> 1083                                
#> 1084                         Tagalog
#> 1085                                
#> 1086                                
#> 1087                                
#> 1088                                
#> 1089                                
#> 1090                          tolışi
#> 1091                          толыши
#> 1092                                
#> 1093                        Setswana
#> 1094                                
#> 1095                  lea faka-Tonga
#> 1096                                
#> 1097                                
#> 1098                       toki pona
#> 1099                       Tok Pisin
#> 1100                          Türkçe
#> 1101                          Ṫuroyo
#> 1102                          Seediq
#> 1103                                
#> 1104                        Xitsonga
#> 1105                                
#> 1106                                
#> 1107                                
#> 1108               татарча / tatarça
#> 1109                         татарча
#> 1110                         tatarça
#> 1111                        Orutooro
#> 1112                                
#> 1113                                
#> 1114                                
#> 1115                      chiTumbuka
#> 1116                                
#> 1117                                
#> 1118                             Twi
#> 1119                                
#> 1120                                
#> 1121                                
#> 1122                                
#> 1123                      reo tahiti
#> 1124                        тыва дыл
#> 1125                                
#> 1126                        ⵜⴰⵎⴰⵣⵉⵖⵜ
#> 1127                          удмурт
#> 1128            ئۇيغۇرچە / Uyghurche
#> 1129                        ئۇيغۇرچە
#> 1130                       Uyghurche
#> 1131                                
#> 1132                      українська
#> 1133                                
#> 1134                                
#> 1135                                
#> 1136                                
#> 1137                                
#> 1138                                
#> 1139                                
#> 1140                            اردو
#> 1141                                
#> 1142             oʻzbekcha / ўзбекча
#> 1143                         ўзбекча
#> 1144                       oʻzbekcha
#> 1145                                
#> 1146                       Tshivenda
#> 1147                          vèneto
#> 1148                     vepsän kel’
#> 1149                      Tiếng Việt
#> 1150                                
#> 1151                      West-Vlams
#> 1152                   Mainfränkisch
#> 1153                        emakhuwa
#> 1154                         Volapük
#> 1155                           Vaďďa
#> 1156                            võro
#> 1157                                
#> 1158                                
#> 1159                           walon
#> 1160                                
#> 1161                        wolaytta
#> 1162                         Winaray
#> 1163                                
#> 1164                                
#> 1165                                
#> 1166                                
#> 1167                       Fakaʻuvea
#> 1168                           waale
#> 1169                           Wolof
#> 1170                                
#> 1171                                
#> 1172                            吴语
#> 1173                    吴语（简体）
#> 1174                    吳語（正體）
#> 1175                                
#> 1176                                
#> 1177                          хальмг
#> 1178                                
#> 1179                        isiXhosa
#> 1180                       მარგალური
#> 1181                                
#> 1182                                
#> 1183                                
#> 1184                                
#> 1185                                
#> 1186                                
#> 1187                                
#> 1188                                
#> 1189                                
#> 1190                        saisiyat
#> 1191                                
#> 1192                                
#> 1193                                
#> 1194                                
#> 1195                                
#> 1196                                
#> 1197                                
#> 1198                           ייִדיש
#> 1199                                
#> 1200                          Yorùbá
#> 1201                                
#> 1202                                
#> 1203                                
#> 1204                                
#> 1205                                
#> 1206                        Nhẽẽgatú
#> 1207                     maaya t’aan
#> 1208                            粵語
#> 1209                    粵语（简体）
#> 1210                    粵語（繁體）
#> 1211                       Vahcuengh
#> 1212                                
#> 1213                                
#> 1214                                
#> 1215                          Zeêuws
#> 1216                                
#> 1217               ⵜⴰⵎⴰⵣⵉⵖⵜ ⵜⴰⵏⴰⵡⴰⵢⵜ
#> 1218               tamaziɣt tanawayt
#> 1219                            中文
#> 1220                            文言
#> 1221                中文（中国大陆）
#> 1222                    中文（简体）
#> 1223                    中文（繁體）
#> 1224                    中文（香港）
#> 1225             閩南語 / Bân-lâm-gí
#> 1226                    中文（澳門）
#> 1227                中文（马来西亚）
#> 1228                  中文（新加坡）
#> 1229                    中文（臺灣）
#> 1230                            粵語
#> 1231                                
#> 1232                                
#> 1233                         isiZulu
#> 1234                                
#> 1235                                
#> 1236                                
```
