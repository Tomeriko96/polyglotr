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
#> 412             fr-ca                            Canadian French
#> 413             fr-ch                               Swiss French
#> 414               frc                               Cajun French
#> 415               frk                                   Frankish
#> 416               frm                              Middle French
#> 417               fro                                 Old French
#> 418               frp                                    Arpitan
#> 419               frr                           Northern Frisian
#> 420               frs                            Eastern Frisian
#> 421               fsl                       French Sign Language
#> 422               fud                                    Futunan
#> 423               fuf                                      Pular
#> 424               fur                                   Friulian
#> 425               fvr                                        Fur
#> 426                fy                            Western Frisian
#> 427                ga                                      Irish
#> 428               gaa                                         Ga
#> 429               gag                                     Gagauz
#> 430               gah                                    Alekano
#> 431               gan                                        Gan
#> 432          gan-hans                Gan (Simplified Han script)
#> 433          gan-hant               Gan (Traditional Han script)
#> 434               gay                                       Gayo
#> 435               gba                                      Gbaya
#> 436               gbb                                   Kaytetye
#> 437               gbk                                      Gaddi
#> 438          gbk-deva                  Gaddi (Devanagari script)
#> 439          gbk-takr                       Gaddi (Takri script)
#> 440               gbm                                   Garhwali
#> 441               gbz                           Zoroastrian Dari
#> 442               gcf                        Guadeloupean Creole
#> 443               gcr                             Guianan Creole
#> 444                gd                            Scottish Gaelic
#> 445               gem                         Germanic languages
#> 446               gez                                       Geez
#> 447               gil                                 Gilbertese
#> 448               gju                                     Gujari
#> 449          gju-arab                     Gujari (Arabic script)
#> 450          gju-deva                 Gujari (Devanagari script)
#> 451                gl                                   Galician
#> 452               gld                                      Nanai
#> 453               glh                           Northwest Pashai
#> 454               glk                                     Gilaki
#> 455               gmh                         Middle High German
#> 456               gml                          Middle Low German
#> 457               gmy                            Mycenaean Greek
#> 458                gn                                    Guarani
#> 459               gnq                                      Ganaʼ
#> 460               goh                            Old High German
#> 461               gom                               Goan Konkani
#> 462          gom-deva           Goan Konkani (Devanagari script)
#> 463          gom-latn                Goan Konkani (Latin script)
#> 464               gon                                      Gondi
#> 465               gor                                  Gorontalo
#> 466               got                                     Gothic
#> 467               gpe                            Ghanaian Pidgin
#> 468               grb                                      Grebo
#> 469               grc                              Ancient Greek
#> 470               gsg                       German Sign Language
#> 471               gsw                                  Alemannic
#> 472            gsw-fr                                   Alsatian
#> 473                gu                                   Gujarati
#> 474               guc                                      Wayuu
#> 475               gum                                  Guambiano
#> 476               gur                                     Frafra
#> 477               guw                                        Gun
#> 478               guz                                      Gusii
#> 479                gv                                       Manx
#> 480               gwi                                   Gwichʼin
#> 481               gya                                      Gbaya
#> 482                ha                                      Hausa
#> 483           ha-arab                      Hausa (Arabic script)
#> 484           ha-latn                       Hausa (Latin script)
#> 485             ha-ne                              Hausa (Niger)
#> 486               hac                                     Gurani
#> 487               hai                                      Haida
#> 488               hak                              Hakka Chinese
#> 489          hak-hans              Hakka (Simplified Han script)
#> 490          hak-hant             Hakka (Traditional Han script)
#> 491          hak-latn                       Hakka (Latin script)
#> 492               hav                                       Havu
#> 493               haw                                   Hawaiian
#> 494               hax                             Southern Haida
#> 495               haz                                   Hazaragi
#> 496               hbo                            Biblical Hebrew
#> 497                he                                     Hebrew
#> 498                hi                                      Hindi
#> 499           hi-kthi                      Hindi (Kaithi script)
#> 500           hi-latn                              Hindi (Latin)
#> 501               hif                                 Fiji Hindi
#> 502          hif-deva             Fiji Hindi (Devanagari script)
#> 503          hif-latn                  Fiji Hindi (Latin script)
#> 504               hil                                 Hiligaynon
#> 505               him                             Western Pahari
#> 506               hit                                    Hittite
#> 507          hit-latn                     Hittite (Latin script)
#> 508          hit-xsux                 Hittite (Cuneiform script)
#> 509               hke                                      Hunde
#> 510               hmn                                      Hmong
#> 511               hne                              Chhattisgarhi
#> 512               hnj                                 Hmong Njua
#> 513               hno                            Northern Hindko
#> 514                ho                                  Hiri Motu
#> 515               hoc                                         Ho
#> 516          hoc-latn                          Ho (Latin script)
#> 517                hr                                   Croatian
#> 518               hrx                                    Hunsrik
#> 519               hsb                              Upper Sorbian
#> 520               hsn                                      Xiang
#> 521          hsn-hans              Xiang (Simplified Han script)
#> 522          hsn-hant             Xiang (Traditional Han script)
#> 523                ht                             Haitian Creole
#> 524               hts                                      Hadza
#> 525                hu                                  Hungarian
#> 526         hu-formal                 Hungarian (formal address)
#> 527               hup                                       Hupa
#> 528               hur                                 Halkomelem
#> 529                hy                                   Armenian
#> 530               hyw                           Western Armenian
#> 531                hz                                     Herero
#> 532                ia                                Interlingua
#> 533               iba                                       Iban
#> 534               ibb                                     Ibibio
#> 535                id                                 Indonesian
#> 536                ie                                Interlingue
#> 537               ifu                             Mayoyao Ifugao
#> 538                ig                                       Igbo
#> 539               igb                                      Ebira
#> 540               igl                                      Igala
#> 541                ii                                 Sichuan Yi
#> 542               ijo                             Ijaw languages
#> 543                ik                                    Inupiaq
#> 544          ike-cans    Eastern Canadian (Aboriginal syllabics)
#> 545          ike-latn            Eastern Canadian (Latin script)
#> 546               ikt                 Western Canadian Inuktitut
#> 547               ilo                                      Iloko
#> 548               inc                       Indo-Aryan languages
#> 549               ine                    Indo-European languages
#> 550               inh                                     Ingush
#> 551                io                                        Ido
#> 552               ira                          Iranian languages
#> 553               iro                        Iroquoian languages
#> 554                is                                  Icelandic
#> 555               ish                                       Esan
#> 556          isk-arab                 Ishkashimi (Arabic script)
#> 557          isk-cyrl               Ishkashimi (Cyrillic script)
#> 558          isk-latn                  Ishkashimi (Latin script)
#> 559               ist                                    Istriot
#> 560               isu                                        Isu
#> 561          isv-cyrl              Interslavic (Cyrillic script)
#> 562          isv-latn                 Interslavic (Latin script)
#> 563                it                                    Italian
#> 564                iu                                  Inuktitut
#> 565               ivb                                     Ibatan
#> 566               izh                                    Ingrian
#> 567                ja                                   Japanese
#> 568           ja-hani                    Japanese (Kanji script)
#> 569           ja-hira                 Japanese (Hiragana script)
#> 570           ja-hrkt                     Japanese (Kana script)
#> 571           ja-kana                 Japanese (Katakana script)
#> 572               jac                                     Popti'
#> 573               jak                                      Jakun
#> 574               jam                    Jamaican Creole English
#> 575               jbo                                     Lojban
#> 576               jdt                                  Judeo-Tat
#> 577          jdt-cyrl                Judeo-Tat (Cyrillic script)
#> 578               jgo                                     Ngomba
#> 579               jje                                       Jeju
#> 580               jmc                                    Machame
#> 581               jpr                              Judeo-Persian
#> 582               jrb                               Judeo-Arabic
#> 583               jut                                     Jutish
#> 584                jv                                   Javanese
#> 585           jv-java                 Javanese (Javanese script)
#> 586                ka                                   Georgian
#> 587               kaa                                Kara-Kalpak
#> 588               kab                                     Kabyle
#> 589               kac                                     Kachin
#> 590               kag                                    Kajaman
#> 591               kai                                   Karekare
#> 592               kaj                                        Jju
#> 593               kam                                      Kamba
#> 594               kar                          Karenic languages
#> 595               kaw                                       Kawi
#> 596               kbd                                  Kabardian
#> 597          kbd-cyrl                Kabardian (Cyrillic script)
#> 598          kbd-latn                   Kabardian (Latin script)
#> 599               kbl                                    Kanembu
#> 600               kbp                                     Kabiye
#> 601               kcg                                       Tyap
#> 602               kck                                    Kalanga
#> 603               kde                                    Makonde
#> 604               kea                               Kabuverdianu
#> 605               kek                                   Qʼeqchiʼ
#> 606               ken                                    Kenyang
#> 607               ker                                       Kera
#> 608               kfo                                       Koro
#> 609               kfr                                     Kutchi
#> 610                kg                                      Kongo
#> 611               kge                                   Komering
#> 612          kge-arab                   Komering (Arabic script)
#> 613               kgg                                    Kusunda
#> 614               kgp                                   Kaingang
#> 615               kha                                      Khasi
#> 616               khi                          Khoisan languages
#> 617               kho                                  Khotanese
#> 618               khq                               Koyra Chiini
#> 619               khw                                     Khowar
#> 620                ki                                     Kikuyu
#> 621               kip                                Sheshi Kham
#> 622               kiu                                  Kirmanjki
#> 623                kj                                   Kuanyama
#> 624               kjh                                     Khakas
#> 625               kjp                                Eastern Pwo
#> 626                kk                                     Kazakh
#> 627           kk-arab                     Kazakh (Arabic script)
#> 628             kk-cn                             Kazakh (China)
#> 629           kk-cyrl                   Kazakh (Cyrillic script)
#> 630             kk-kz                        Kazakh (Kazakhstan)
#> 631           kk-latn                      Kazakh (Latin script)
#> 632             kk-tr                            Kazakh (Turkey)
#> 633               kkj                                       Kako
#> 634                kl                                Kalaallisut
#> 635               kld                                 Gamilaraay
#> 636               kln                                   Kalenjin
#> 637               kls                                    Kalasha
#> 638          kls-arab                    Kalasha (Arabic script)
#> 639          kls-latn                     Kalasha (Latin script)
#> 640                km                                      Khmer
#> 641               kmb                                   Kimbundu
#> 642               kmr                           Northern Kurdish
#> 643          kmr-arab           Northern Kurdish (Arabic script)
#> 644          kmr-latn            Northern Kurdish (Latin script)
#> 645               kmz                           Khorasani Turkic
#> 646                kn                                    Kannada
#> 647               knc                             Central Kanuri
#> 648               kne                                  Kankanaey
#> 649               knn                      Maharashtrian Konkani
#> 650               knq                                     Kintaq
#> 651                ko                                     Korean
#> 652             ko-cn                             Korean (China)
#> 653           ko-hani                      Korean (Hanja script)
#> 654           ko-kore                      Korean (mixed script)
#> 655             ko-kp                       Korean (North Korea)
#> 656               koi                               Komi-Permyak
#> 657               kok                                    Konkani
#> 658               kos                                   Kosraean
#> 659               koy                                    Koyukon
#> 660               kpe                                     Kpelle
#> 661               kqr                                 Kimaragang
#> 662               kqt                        Klias River Kadazan
#> 663               kqv                                     Okolod
#> 664                kr                                     Kanuri
#> 665               krc                            Karachay-Balkar
#> 666               kri                                       Krio
#> 667               krj                                  Kinaray-a
#> 668               krl                                   Karelian
#> 669               kro                              Kru languages
#> 670               kru                                     Kurukh
#> 671                ks                                   Kashmiri
#> 672           ks-arab                   Kashmiri (Arabic script)
#> 673           ks-deva               Kashmiri (Devanagari script)
#> 674               ksb                                   Shambala
#> 675               ksf                                      Bafia
#> 676               ksh                                  Colognian
#> 677               ksw                                S'gaw Karen
#> 678          ksy-beng               Kharia Thar (Bengali script)
#> 679                ku                                    Kurdish
#> 680           ku-arab                    Kurdish (Arabic script)
#> 681           ku-latn                     Kurdish (Latin script)
#> 682               kum                                      Kumyk
#> 683               kus                                     Kusaal
#> 684               kut                                    Kutenai
#> 685                kv                                       Komi
#> 686               kve                                  Kalabakan
#> 687                kw                                    Cornish
#> 688               kwk                                  Kwakʼwala
#> 689               kxd                               Brunei Malay
#> 690               kxi                             Keningau Murut
#> 691               kxn                                    Kanowit
#> 692               kxv                                       Kuvi
#> 693                ky                                     Kyrgyz
#> 694          kyw-beng                   Kurmali (Bengali script)
#> 695          kyw-deva                Kurmali (Devanagari script)
#> 696                la                                      Latin
#> 697               lad                                     Ladino
#> 698          lad-hebr                     Ladino (Hebrew script)
#> 699          lad-latn                      Ladino (Latin script)
#> 700               lag                                      Langi
#> 701               lah                            Western Panjabi
#> 702               laj                                      Lango
#> 703               lam                                      Lamba
#> 704                lb                              Luxembourgish
#> 705               lbe                                        Lak
#> 706               lcm                                     Tungag
#> 707               ldn                                     Láadan
#> 708               lem                                   Nomaande
#> 709               lez                                   Lezghian
#> 710               lfn                         Lingua Franca Nova
#> 711                lg                                      Ganda
#> 712                li                                 Limburgish
#> 713               lij                                   Ligurian
#> 714            lij-mc                                 Monégasque
#> 715               lil                                   Lillooet
#> 716               liv                                   Livonian
#> 717               ljp                                Lampung Api
#> 718               lki                                       Laki
#> 719               lkt                                     Lakota
#> 720               lld                                      Ladin
#> 721               lmn                                    Lambadi
#> 722               lmo                                    Lombard
#> 723                ln                                    Lingala
#> 724               lns                                    Lamnso'
#> 725                lo                                        Lao
#> 726               lol                                      Mongo
#> 727               lou                           Louisiana Creole
#> 728               loz                                       Lozi
#> 729               lrc                              Northern Luri
#> 730               lsm                                     Saamia
#> 731                lt                                 Lithuanian
#> 732               ltg                                  Latgalian
#> 733                lu                               Luba-Katanga
#> 734               lua                                 Luba-Lulua
#> 735               lud                                      Ludic
#> 736               lui                                    Luiseno
#> 737               lun                                      Lunda
#> 738               luo                                        Luo
#> 739               lus                                       Mizo
#> 740               lut                                Lushootseed
#> 741               luy                                      Luyia
#> 742               luz                              Southern Luri
#> 743                lv                                    Latvian
#> 744               lzh                           Literary Chinese
#> 745               lzz                                        Laz
#> 746               mad                                   Madurese
#> 747               maf                                       Mafa
#> 748               mag                                     Magahi
#> 749               mai                                   Maithili
#> 750               mak                                    Makasar
#> 751          mak-bugi                  Makasar (Buginese script)
#> 752               man                                   Mandingo
#> 753               map                     Austronesian languages
#> 754           map-bms                                 Banyumasan
#> 755               mas                                      Masai
#> 756               maw                                   Mampruli
#> 757               mcn                                      Massa
#> 758               mcp                                       Maka
#> 759               mde                                       Maba
#> 760               mdf                                     Moksha
#> 761               mdh                               Maguindanaon
#> 762               mdr                                     Mandar
#> 763               men                                      Mende
#> 764               mer                                       Meru
#> 765               mey                                 Hassaniyya
#> 766               mfa                     Kelantan-Pattani Malay
#> 767               mfe                                   Morisyen
#> 768                mg                                   Malagasy
#> 769               mga                               Middle Irish
#> 770               mgh                             Makhuwa-Meetto
#> 771               mgo                                      Metaʼ
#> 772                mh                                Marshallese
#> 773               mhk                                    Mungaka
#> 774               mhn                                    Mòcheno
#> 775               mhr                               Eastern Mari
#> 776                mi                                      Māori
#> 777               mic                                    Mi'kmaw
#> 778               mid                                    Mandaic
#> 779               min                                Minangkabau
#> 780               miq                                    Miskito
#> 781               mis                       unsupported language
#> 782               mix                                     Mixtec
#> 783          mjx-beng                    Mahali (Bengali script)
#> 784                mk                                 Macedonian
#> 785               mkh                                  Mon-Khmer
#> 786                ml                                  Malayalam
#> 787                mn                                  Mongolian
#> 788           mn-cyrl                Mongolian (Cyrillic script)
#> 789           mn-mong               Mongolian (Mongolian script)
#> 790               mnc                                     Manchu
#> 791          mnc-latn                      Manchu (Latin script)
#> 792          mnc-mong                  Manchu (Mongolian script)
#> 793               mni                                   Manipuri
#> 794          mni-beng                  Manipuri (Bengali script)
#> 795               mnj                                      Munji
#> 796               mno                           Manobo languages
#> 797               mnq                                     Minriq
#> 798               mns                                      Mansi
#> 799               mnw                                        Mon
#> 800                mo                                   Moldovan
#> 801               moe                                 Innu-aimun
#> 802               moh                                     Mohawk
#> 803               mos                                      Mossi
#> 804                mr                                    Marathi
#> 805           mr-modi                      Marathi (Modi script)
#> 806               mrh                                       Mara
#> 807               mrj                               Western Mari
#> 808               mrt                             Marghi Central
#> 809               mrv                                  Mangareva
#> 810                ms                                      Malay
#> 811           ms-arab                        Malay (Jawi script)
#> 812               msi                                Sabah Malay
#> 813                mt                                    Maltese
#> 814               mua                                    Mundang
#> 815               mui                                       Musi
#> 816               mul                         multiple languages
#> 817               mun                            Munda languages
#> 818               mus                                   Muscogee
#> 819               mvf                       Peripheral Mongolian
#> 820               mvi                                     Miyako
#> 821          mvi-hira                   Miyako (Hiragana script)
#> 822               mvv                                      Tagol
#> 823               mwl                                  Mirandese
#> 824               mwr                                    Marwari
#> 825               mwv                                   Mentawai
#> 826               mww                                  Hmong Daw
#> 827          mww-latn                   Hmong Daw (Latin script)
#> 828                my                                    Burmese
#> 829               mye                                      Myene
#> 830               myn                            Mayan languages
#> 831               myv                                      Erzya
#> 832               mzn                                Mazanderani
#> 833                na                                      Nauru
#> 834               nah                                    Nahuatl
#> 835               nai      Indigenous languages of North America
#> 836               nan                                     Minnan
#> 837          nan-hani                        Minnan (Han script)
#> 838          nan-hans             Minnan (Simplified Han script)
#> 839          nan-hant            Minnan (Traditional Han script)
#> 840  nan-latn-pehoeji                         Minnan (Pe̍h-ōe-jī)
#> 841    nan-latn-tailo                            Minnan (Tâi-lô)
#> 842               nap                                 Neapolitan
#> 843               naq                                       Nama
#> 844                nb                           Norwegian Bokmål
#> 845                nd                              North Ndebele
#> 846               nds                                 Low German
#> 847            nds-nl                                  Low Saxon
#> 848                ne                                     Nepali
#> 849               new                                     Newari
#> 850                ng                                     Ndonga
#> 851               nge                                     Ngémba
#> 852               nia                                       Nias
#> 853               nic                      Niger–Congo languages
#> 854               nit                        Southeastern Kolami
#> 855               niu                                     Niuean
#> 856               njo                                    Ao Naga
#> 857                nl                                      Dutch
#> 858             nl-be                                    Flemish
#> 859       nl-informal                   Dutch (informal address)
#> 860               nla                                   Ngombala
#> 861               nmg                                     Kwasio
#> 862               nmz                                      Nawdm
#> 863                nn                          Norwegian Nynorsk
#> 864       nn-hognorsk                         Norwegian Høgnorsk
#> 865               nnh                                  Ngiemboon
#> 866               nnz                                   Nda'Nda'
#> 867                no                                  Norwegian
#> 868               nod                              Northern Thai
#> 869          nod-thai                Northern Thai (Thai script)
#> 870               nog                                      Nogai
#> 871               non                                  Old Norse
#> 872          non-runr                   Old Norse (Runic script)
#> 873               nov                                     Novial
#> 874               nqo                                       N’Ko
#> 875                nr                              South Ndebele
#> 876            nrf-gg                                Guernésiais
#> 877            nrf-je                                   Jèrriais
#> 878               nrm                                     Norman
#> 879               nsk                                    Naskapi
#> 880               nsl                    Norwegian Sign Language
#> 881               nso                             Northern Sotho
#> 882               ntd                             Sesayap Tidung
#> 883               nub                           Nubian languages
#> 884               nup                                       Nupe
#> 885               nus                                       Nuer
#> 886                nv                                     Navajo
#> 887               nwc                           Classical Newari
#> 888               nxm                                   Numidian
#> 889                ny                                     Nyanja
#> 890               nym                                   Nyamwezi
#> 891               nyn                                   Nyankole
#> 892               nyo                                      Nyoro
#> 893               nys                                    Nyungar
#> 894               nzi                                      Nzima
#> 895               obt                                 Old Breton
#> 896                oc                                    Occitan
#> 897               oco                                Old Cornish
#> 898               odt                                  Old Dutch
#> 899               ofs                                Old Frisian
#> 900                oj                                     Ojibwa
#> 901               ojb                        Northwestern Ojibwa
#> 902               ojc                             Central Ojibwa
#> 903               ojp                               Old Japanese
#> 904          ojp-hani                Old Japanese (Kanji script)
#> 905          ojp-hira             Old Japanese (Hiragana script)
#> 906               ojs                                   Oji-Cree
#> 907               ojw                             Western Ojibwa
#> 908               oka                                   Okanagan
#> 909               olo                             Livvi-Karelian
#> 910                om                                      Oromo
#> 911               oma                                Omaha-Ponca
#> 912               ood                                    O'odham
#> 913                or                                       Odia
#> 914                os                                    Ossetic
#> 915               osa                                      Osage
#> 916          osa-latn                       Osage (Latin script)
#> 917               osi                                      Osing
#> 918               osx                                  Old Saxon
#> 919               ota                            Ottoman Turkish
#> 920               otk                                Old Turkish
#> 921               oto                          Otomian languages
#> 922               ovd                                  Elfdalian
#> 923               owl                                  Old Welsh
#> 924                pa                                    Punjabi
#> 925           pa-guru                  Punjabi (Gurmukhi script)
#> 926               paa                           Papuan languages
#> 927               pag                                 Pangasinan
#> 928               pal                                    Pahlavi
#> 929          pal-phli     Pahlavi (Inscriptional Pahlavi script)
#> 930          pal-phlp           Pahlavi (Psalter Pahlavi script)
#> 931          pal-phlv              Pahlavi (Book Pahlavi script)
#> 932               pam                                   Pampanga
#> 933               pao                            Northern Paiute
#> 934               pap                                 Papiamento
#> 935            pap-aw                         Papiamento (Aruba)
#> 936               paq                                      Parya
#> 937               pau                                    Palauan
#> 938               pbb                                       Páez
#> 939               pcd                                     Picard
#> 940               pcm                            Nigerian Pidgin
#> 941               pdc                        Pennsylvania German
#> 942               pdt                               Plautdietsch
#> 943               peo                                Old Persian
#> 944               pfl                            Palatine German
#> 945               pgd                                   Gāndhārī
#> 946          pgd-arab                   Gāndhārī (Arabic script)
#> 947          pgd-deva               Gāndhārī (Devanagari script)
#> 948          pgd-khar               Gāndhārī (Kharoshthi script)
#> 949               pgl                            Primitive Irish
#> 950               phi                       Philippine languages
#> 951               phl                                     Palula
#> 952               phn                                 Phoenician
#> 953          phn-latn                  Phoenician (Latin script)
#> 954          phn-phnx             Phoenician (Phoenician script)
#> 955               phr                             Pahari-Potwari
#> 956                pi                                       Pali
#> 957           pi-sidd                      Pali (Siddham script)
#> 958               pih                           Pitcairn-Norfolk
#> 959               pis                                      Pijin
#> 960               pjt                             Pitjantjatjara
#> 961               pkc                                    Paekche
#> 962               pko                                     Pökoot
#> 963               pks                     Pakistan Sign Language
#> 964                pl                                     Polish
#> 965               plv                         Southwest Palawano
#> 966               plw                    Brooke's Point Palawano
#> 967               pms                                Piedmontese
#> 968               pnb                            Western Punjabi
#> 969               pnt                                     Pontic
#> 970               pon                                  Pohnpeian
#> 971               pov                       Upper Guinea Crioulo
#> 972               ppl                                      Nawat
#> 973               ppu                              Papora-Hoanya
#> 974               pqm                     Maliseet-Passamaquoddy
#> 975               pra                                    Prakrit
#> 976               prc                                    Parachi
#> 977               prg                                   Prussian
#> 978               pro                              Old Provençal
#> 979               prs                                       Dari
#> 980                ps                                     Pashto
#> 981             ps-af                       Pashto (Afghanistan)
#> 982             ps-pk                          Pashto (Pakistan)
#> 983               psh                           Southwest Pashai
#> 984               psi                           Southeast Pashai
#> 985               psu                          Sauraseni Prākrit
#> 986          psu-arab          Sauraseni Prākrit (Arabic script)
#> 987          psu-brah          Sauraseni Prākrit (Brahmi script)
#> 988          psu-deva      Sauraseni Prākrit (Devanagari script)
#> 989          psu-guru        Sauraseni Prākrit (Gurmukhi script)
#> 990                pt                                 Portuguese
#> 991         pt-ao1990   Portuguese (1990 Orthographic Agreement)
#> 992             pt-br                       Brazilian Portuguese
#> 993       pt-colb1945   Portuguese (1945 Orthographic Agreement)
#> 994             pt-pt                        European Portuguese
#> 995               pwn                                     Paiwan
#> 996               pyu                                     Puyuma
#> 997                qu                                    Quechua
#> 998               quc                                    Kʼicheʼ
#> 999               qug                Chimborazo Highland Quichua
#> 1000              qwh                     Huaylas Ancash Quechua
#> 1001              qxp                               Puno Quechua
#> 1002              qxq                                    Qashqai
#> 1003              qya                                     Quenya
#> 1004              rag                                    Logooli
#> 1005              rah                                      Rabha
#> 1006              raj                                 Rajasthani
#> 1007              rap                                    Rapanui
#> 1008              rar                                 Rarotongan
#> 1009              rcf                      Réunion Creole French
#> 1010              rej                                     Rejang
#> 1011              rgn                                   Romagnol
#> 1012              rhg                                   Rohingya
#> 1013         rhg-arab                   Rohingya (Arabic script)
#> 1014         rhg-rohg          Rohingya (Hanifi Rohingya script)
#> 1015              rif                                    Riffian
#> 1016              rki                                  Arakanese
#> 1017              rkt                                   Rangpuri
#> 1018               rm                                    Romansh
#> 1019         rm-puter                                      Putèr
#> 1020         rm-rumgr                         Rumantsch Grischun
#> 1021      rm-surmiran                                   Surmiran
#> 1022       rm-sursilv                                  Sursilvan
#> 1023       rm-sutsilv                                  Sutsilvan
#> 1024      rm-vallader                                   Vallader
#> 1025              rmc                          Carpathian Romani
#> 1026              rmf                               Finnish Kalo
#> 1027              rmg                        Traveller Norwegian
#> 1028              rml                              Baltic Romani
#> 1029         rml-cyrl            Baltic Romani (Cyrillic script)
#> 1030              rmn                              Balkan Romani
#> 1031              rmo                               Sinte Romani
#> 1032              rmw                               Welsh-Romani
#> 1033              rmy                                Vlax Romani
#> 1034               rn                                      Rundi
#> 1035               ro                                   Romanian
#> 1036            ro-md                                  Moldavian
#> 1037              roa                          Romance languages
#> 1038          roa-rup                                  Aromanian
#> 1039         roa-tara                                  Tarantino
#> 1040              rof                                      Rombo
#> 1041              rom                                     Romany
#> 1042              rsk                            Pannonian Rusyn
#> 1043              rtm                                    Rotuman
#> 1044               ru                                    Russian
#> 1045      ru-petr1708              Russian (Petrine orthography)
#> 1046              rue                                      Rusyn
#> 1047              rug                                    Roviana
#> 1048              ruo                             Istro Romanian
#> 1049              rup                                  Aromanian
#> 1050              ruq                           Megleno-Romanian
#> 1051         ruq-cyrl         Megleno-Romanian (Cyrillic script)
#> 1052         ruq-latn            Megleno-Romanian (Latin script)
#> 1053              rut                                      Rutul
#> 1054               rw                                Kinyarwanda
#> 1055              rwk                                        Rwa
#> 1056              rwr                            Marwari (India)
#> 1057              rys                                    Yaeyama
#> 1058         rys-hira                  Yaeyama (Hiragana script)
#> 1059              ryu                                   Okinawan
#> 1060         ryu-hira                 Okinawan (Hiragana script)
#> 1061               sa                                   Sanskrit
#> 1062          sa-sidd                  Sanskrit (Siddham script)
#> 1063              sad                                    Sandawe
#> 1064              sah                                      Yakut
#> 1065              sai        South American indigenous languages
#> 1066              sal                         Salishan languages
#> 1067              sam                          Samaritan Aramaic
#> 1068              saq                                    Samburu
#> 1069              sas                                      Sasak
#> 1070              sat                                    Santali
#> 1071         sat-beng                   Santali (Bengali script)
#> 1072         sat-latn                     Santali (Latin script)
#> 1073         sat-orya                     Santali (Oriya script)
#> 1074              saz                                 Saurashtra
#> 1075              sba                                    Ngambay
#> 1076              sbp                                      Sangu
#> 1077               sc                                  Sardinian
#> 1078              scl                                      Shina
#> 1079              scn                                   Sicilian
#> 1080              sco                                      Scots
#> 1081               sd                                     Sindhi
#> 1082          sd-deva                 Sindhi (Devanagari script)
#> 1083          sd-gujr                   Sindhi (Gujarati script)
#> 1084          sd-khoj                     Sindhi (Khojki script)
#> 1085          sd-sind                  Sindhi (Khudawadi script)
#> 1086              sdc                        Sassarese Sardinian
#> 1087              sdh                           Southern Kurdish
#> 1088         sdh-arab           Southern Kurdish (Arabic script)
#> 1089         sdh-latn            Southern Kurdish (Latin script)
#> 1090              sdo                               Bukar–Sadong
#> 1091               se                              Northern Sami
#> 1092            se-fi                    Northern Sami (Finland)
#> 1093            se-no                     Northern Sami (Norway)
#> 1094            se-se                     Northern Sami (Sweden)
#> 1095              sea                                      Semai
#> 1096              see                                     Seneca
#> 1097              seh                                       Sena
#> 1098              sei                                       Seri
#> 1099              sel                                     Selkup
#> 1100              sem                          Semitic languages
#> 1101              ser                                    Serrano
#> 1102              ses                            Koyraboro Senni
#> 1103               sg                                      Sango
#> 1104              sga                                  Old Irish
#> 1105              sgh                                    Shughni
#> 1106         sgh-arab                    Shughni (Arabic script)
#> 1107         sgh-cyrl                  Shughni (Cyrillic script)
#> 1108         sgh-latn                     Shughni (Latin script)
#> 1109              sgn                             sign languages
#> 1110              sgs                                 Samogitian
#> 1111         sgy-arab                  Sanglechi (Arabic script)
#> 1112         sgy-latn                   Sanglechi (Latin script)
#> 1113               sh                             Serbo-Croatian
#> 1114          sh-cyrl           Serbo-Croatian (Cyrillic script)
#> 1115          sh-latn              Serbo-Croatian (Latin script)
#> 1116              shd                               Kundal Shahi
#> 1117              shi                                  Tachelhit
#> 1118         shi-latn                   Tachelhit (Latin script)
#> 1119         shi-tfng                Tachelhit (Tifinagh script)
#> 1120              shn                                       Shan
#> 1121              shu                             Chadian Arabic
#> 1122              shy                                    Shawiya
#> 1123         shy-arab                    Shawiya (Arabic script)
#> 1124         shy-latn                     Shawiya (Latin script)
#> 1125         shy-tfng                  Shawiya (Tifinagh script)
#> 1126               si                                    Sinhala
#> 1127              sia                                Akkala Sami
#> 1128              sid                                     Sidamo
#> 1129           simple                             Simple English
#> 1130              sio                           Siouan languages
#> 1131              sit                     Sino-Tibetan languages
#> 1132              sjd                                Kildin Sami
#> 1133              sje                                  Pite Sami
#> 1134              sjk                                  Kemi Sami
#> 1135              sjn                                   Sindarin
#> 1136              sjo                                       Xibe
#> 1137              sjt                                   Ter Sami
#> 1138              sju                                   Ume Sami
#> 1139               sk                                     Slovak
#> 1140              skr                                    Saraiki
#> 1141         skr-arab                    Saraiki (Arabic script)
#> 1142               sl                                  Slovenian
#> 1143              sla                           Slavic languages
#> 1144              slh                       Southern Lushootseed
#> 1145              sli                             Lower Silesian
#> 1146              slr                                      Salar
#> 1147              sly                                    Selayar
#> 1148               sm                                     Samoan
#> 1149              sma                              Southern Sami
#> 1150              smi                             Sámi languages
#> 1151              smj                                  Lule Sami
#> 1152              smn                                 Inari Sami
#> 1153              sms                                 Skolt Sami
#> 1154               sn                                      Shona
#> 1155              sne                                      Jagoi
#> 1156              snk                                    Soninke
#> 1157               so                                     Somali
#> 1158              sog                                    Sogdien
#> 1159              son                          Songhay languages
#> 1160              spv                                 Sambalpuri
#> 1161               sq                                   Albanian
#> 1162               sr                                    Serbian
#> 1163          sr-cyrl                  Serbian (Cyrillic script)
#> 1164            sr-ec                  Serbian (Cyrillic script)
#> 1165            sr-el                     Serbian (Latin script)
#> 1166          sr-latn                     Serbian (Latin script)
#> 1167            sr-me                                Montenegrin
#> 1168         srh-arab                   Sarikoli (Arabic script)
#> 1169         srh-cyrl                 Sarikoli (Cyrillic script)
#> 1170         srh-latn                    Sarikoli (Latin script)
#> 1171              srk                                   Serudung
#> 1172              srn                               Sranan Tongo
#> 1173              sro                      Campidanese Sardinian
#> 1174              srq                                    Sirionó
#> 1175              srr                                      Serer
#> 1176               ss                                      Swati
#> 1177              ssa                     Nilo-Saharan languages
#> 1178              ssb                              Southern Sama
#> 1179              ssf                                       Thao
#> 1180              ssy                                       Saho
#> 1181               st                             Southern Sotho
#> 1182              sth                                     Shelta
#> 1183              stq                          Saterland Frisian
#> 1184              str                             Straits Salish
#> 1185              sty                             Siberian Tatar
#> 1186               su                                  Sundanese
#> 1187              suk                                     Sukuma
#> 1188              sus                                       Susu
#> 1189              sux                                   Sumerian
#> 1190         sux-latn                    Sumerian (Latin script)
#> 1191         sux-xsux                Sumerian (Cuneiform script)
#> 1192              suz                                     Sunwar
#> 1193               sv                                    Swedish
#> 1194              sva                                       Svan
#> 1195               sw                                    Swahili
#> 1196            sw-cd                              Congo Swahili
#> 1197              swb                                   Comorian
#> 1198              sxr                                     Saaroa
#> 1199              sxu                                Upper Saxon
#> 1200              syc                           Classical Syriac
#> 1201              syl                                    Sylheti
#> 1202         syl-beng                   Sylheti (Bengali script)
#> 1203         syl-sylo             Sylheti (Sylheti Nagri script)
#> 1204              syr                                     Syriac
#> 1205              szl                                   Silesian
#> 1206              szy                                   Sakizaya
#> 1207               ta                                      Tamil
#> 1208              tai                              Tai languages
#> 1209              tao                                       Yami
#> 1210              tay                                     Atayal
#> 1211              tbl                                      Tboli
#> 1212              tce                          Southern Tutchone
#> 1213              tcy                                       Tulu
#> 1214              tdd                                   Tai Nuea
#> 1215               te                                     Telugu
#> 1216              tem                                      Timne
#> 1217              teo                                       Teso
#> 1218              ter                                     Tereno
#> 1219              tet                                      Tetum
#> 1220               tg                                      Tajik
#> 1221          tg-cyrl                    Tajik (Cyrillic script)
#> 1222          tg-latn                       Tajik (Latin script)
#> 1223              tgx                                     Tagish
#> 1224               th                                       Thai
#> 1225              thq                              Kochila Tharu
#> 1226              thr                                 Rana Tharu
#> 1227              tht                                    Tahltan
#> 1228               ti                                   Tigrinya
#> 1229              tig                                      Tigre
#> 1230              tih                                    Timugon
#> 1231              tiv                                        Tiv
#> 1232              tji                             Northern Tujia
#> 1233               tk                                    Turkmen
#> 1234              tkl                                  Tokelauan
#> 1235              tkr                                    Tsakhur
#> 1236               tl                                    Tagalog
#> 1237              tlb                                     Tobelo
#> 1238              tlh                                    Klingon
#> 1239         tlh-latn                     Klingon (Latin script)
#> 1240         tlh-piqd                   Klingon (Klingon script)
#> 1241              tli                                    Tlingit
#> 1242              tly                                     Talysh
#> 1243         tly-cyrl                   Talysh (Cyrillic script)
#> 1244              tmh                                   Tamashek
#> 1245              tmr                  Jewish Babylonian Aramaic
#> 1246               tn                                     Tswana
#> 1247              tnq                                      Taíno
#> 1248               to                                     Tongan
#> 1249              tog                                Nyasa Tonga
#> 1250              toi                            Tonga (Botatwe)
#> 1251              tok                                  Toki Pona
#> 1252              tpi                                  Tok Pisin
#> 1253               tr                                    Turkish
#> 1254              trp                                   Kokborok
#> 1255              tru                                     Turoyo
#> 1256              trv                                     Taroko
#> 1257              trw                                    Torwali
#> 1258               ts                                     Tsonga
#> 1259              tsd                                  Tsakonian
#> 1260              tsg                                     Tausug
#> 1261              tsi                                  Tsimshian
#> 1262              tsu                                      Tsou 
#> 1263               tt                                      Tatar
#> 1264          tt-cyrl                    Tatar (Cyrillic script)
#> 1265          tt-latn                       Tatar (Latin script)
#> 1266              ttj                                      Tooro
#> 1267              ttm                          Northern Tutchone
#> 1268              ttt                                 Muslim Tat
#> 1269              tui                                     Tupuri
#> 1270              tum                                    Tumbuka
#> 1271              tup                           Tupian languages
#> 1272              tut                           Altaic languages
#> 1273              tvl                                     Tuvalu
#> 1274              tvu                                      Tunen
#> 1275               tw                                        Twi
#> 1276              twd                                    Tweants
#> 1277              twq                                    Tasawaq
#> 1278              txa                                  Tombonuwo
#> 1279              txg                                     Tangut
#> 1280         txo-beng                      Toto (Bengali script)
#> 1281         txo-toto                         Toto (Toto script)
#> 1282              txx                                     Tatana
#> 1283               ty                                   Tahitian
#> 1284              tyv                                   Tuvinian
#> 1285              tzl                                   Talossan
#> 1286              tzm                    Central Atlas Tamazight
#> 1287              udm                                     Udmurt
#> 1288               ug                                     Uyghur
#> 1289          ug-arab                     Uyghur (Arabic script)
#> 1290          ug-cyrl                   Uyghur (Cyrillic script)
#> 1291          ug-latn                      Uyghur (Latin script)
#> 1292              uga                                   Ugaritic
#> 1293               uk                                  Ukrainian
#> 1294              ulc                                       Ulch
#> 1295              uln                               Unserdeutsch
#> 1296              umb                                    Umbundu
#> 1297              umu                                     Munsee
#> 1298              und                      undetermined language
#> 1299              unr                                    Mundari
#> 1300         unr-deva                Mundari (Devanagari script)
#> 1301         unr-nagm               Mundari (Nag Mundari script)
#> 1302               ur                                       Urdu
#> 1303              urk                                Urak Lawoiʼ
#> 1304              ush                                     Ushoji
#> 1305              uun                                      Pazeh
#> 1306               uz                                      Uzbek
#> 1307          uz-cyrl                    Uzbek (Cyrillic script)
#> 1308          uz-latn                       Uzbek (Latin script)
#> 1309              vai                                        Vai
#> 1310               ve                                      Venda
#> 1311              vec                                   Venetian
#> 1312              vep                                       Veps
#> 1313               vi                                 Vietnamese
#> 1314          vi-hani                    Vietnamese (Han script)
#> 1315              vls                               West Flemish
#> 1316              vmf                            Main-Franconian
#> 1317              vmw                                    Makhuwa
#> 1318               vo                                    Volapük
#> 1319              vot                                      Votic
#> 1320              vro                                       Võro
#> 1321              vun                                      Vunjo
#> 1322              vut                                       Vute
#> 1323               wa                                    Walloon
#> 1324              wae                                     Walser
#> 1325              wak                         Wakashan languages
#> 1326              wal                                   Wolaytta
#> 1327              war                                      Waray
#> 1328              was                                      Washo
#> 1329         wbl-arab                      Wakhi (Arabic script)
#> 1330      wbl-arab-af         Wakhi (Arabic script, Afghanistan)
#> 1331      wbl-arab-cn               Wakhi (Arabic script, China)
#> 1332      wbl-arab-pk            Wakhi (Arabic script, Pakistan)
#> 1333         wbl-cyrl                    Wakhi (Cyrillic script)
#> 1334         wbl-latn                       Wakhi (Latin script)
#> 1335              wbp                                   Warlpiri
#> 1336              wen                          Sorbian languages
#> 1337              wes                          Pidgin (Cameroon)
#> 1338              wlm                               Middle Welsh
#> 1339              wls                                  Wallisian
#> 1340              wlx                                       Wali
#> 1341               wo                                      Wolof
#> 1342              wsg                             Adilabad Gondi
#> 1343              wsv                        Wotapuri-Katarqalai
#> 1344              wuu                                         Wu
#> 1345         wuu-hans                 Wu (Simplified Han script)
#> 1346         wuu-hant                Wu (Traditional Han script)
#> 1347              wya                                    Wyandot
#> 1348              wyi                                 Woiwurrung
#> 1349              xal                                     Kalmyk
#> 1350              xbm                              Middle Breton
#> 1351               xh                                      Xhosa
#> 1352              xmf                                 Mingrelian
#> 1353              xmm                               Manado Malay
#> 1354              xnb                                 Kanakanavu
#> 1355              xno                               Anglo-Norman
#> 1356              xnr                                     Kangri
#> 1357         xnr-deva                 Kangri (Devanagari script)
#> 1358         xnr-takr                      Kangri (Takri script)
#> 1359              xog                                       Soga
#> 1360              xon                                   Konkomba
#> 1361              xpu                                      Punic
#> 1362              xsu                                     Sanumá
#> 1363              xsy                                   Saisiyat
#> 1364         yah-cyrl               Yazghulami (Cyrillic script)
#> 1365         yah-latn                  Yazghulami (Latin script)
#> 1366         yai-cyrl                 Yaghnobi (Cyrillic script)
#> 1367         yai-latn                    Yaghnobi (Latin script)
#> 1368              yao                                        Yao
#> 1369              yap                                     Yapese
#> 1370              yas                                     Nugunu
#> 1371              yat                                    Yambeta
#> 1372              yav                                    Yangben
#> 1373              ybb                                      Yemba
#> 1374              ydd                            Eastern Yiddish
#> 1375              ydg                                     Yidgha
#> 1376              yec                                    Yeniche
#> 1377               yi                                    Yiddish
#> 1378              ykg                            Tundra Yukaghir
#> 1379               yo                                     Yoruba
#> 1380              yoi                                   Yonaguni
#> 1381         yoi-hira                 Yonaguni (Hiragana script)
#> 1382              yox                                      Yoron
#> 1383         yox-hira                    Yoron (Hiragana script)
#> 1384              ypk                            Yupik languages
#> 1385              yrk                                     Nenets
#> 1386              yrl                                  Nheengatu
#> 1387              yua                               Yucatec Maya
#> 1388              yue                                  Cantonese
#> 1389         yue-hans          Cantonese (Simplified Han script)
#> 1390         yue-hant         Cantonese (Traditional Han script)
#> 1391               za                                     Zhuang
#> 1392              zai                            Isthmus Zapotec
#> 1393              zap                                    Zapotec
#> 1394              zbl                                Blissymbols
#> 1395              zea                                  Zeelandic
#> 1396              zen                                     Zenaga
#> 1397              zgh                Standard Moroccan Tamazight
#> 1398         zgh-latn Standard Moroccan Tamazight (Latin script)
#> 1399               zh                                    Chinese
#> 1400     zh-classical                           Literary Chinese
#> 1401            zh-cn                            Chinese (China)
#> 1402          zh-hans                         Simplified Chinese
#> 1403          zh-hant                        Traditional Chinese
#> 1404            zh-hk                        Chinese (Hong Kong)
#> 1405       zh-min-nan                                     Minnan
#> 1406            zh-mo                            Chinese (Macau)
#> 1407            zh-my                         Chinese (Malaysia)
#> 1408            zh-sg                        Chinese (Singapore)
#> 1409            zh-tw                           Chinese (Taiwan)
#> 1410           zh-yue                                  Cantonese
#> 1411              zmi                      Negeri Sembilan Malay
#> 1412              znd                            Zande languages
#> 1413              zpu                            Yalálag Zapotec
#> 1414               zu                                       Zulu
#> 1415              zun                                       Zuni
#> 1416              zxx                      no linguistic content
#> 1417              zza                                       Zaza
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
#> 38                                  
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
#> 414                  français cadien
#> 415                                 
#> 416                                 
#> 417                                 
#> 418                          arpetan
#> 419                       Nordfriisk
#> 420                                 
#> 421                                 
#> 422                                 
#> 423                                 
#> 424                           furlan
#> 425                   poor’íŋ belé’ŋ
#> 426                            Frysk
#> 427                          Gaeilge
#> 428                               Ga
#> 429                           Gagauz
#> 430                                 
#> 431                             贛語
#> 432                     赣语（简体）
#> 433                     贛語（繁體）
#> 434                                 
#> 435                                 
#> 436                                 
#> 437                                 
#> 438                                 
#> 439                                 
#> 440                                 
#> 441                                 
#> 442                  kréyòl Gwadloup
#> 443                 kriyòl gwiyannen
#> 444                         Gàidhlig
#> 445                                 
#> 446                                 
#> 447                                 
#> 448                                 
#> 449                                 
#> 450                                 
#> 451                           galego
#> 452                             на̄ни
#> 453                                 
#> 454                            گیلکی
#> 455                                 
#> 456                                 
#> 457                                 
#> 458                          Avañe'ẽ
#> 459                                 
#> 460                                 
#> 461     गोंयची कोंकणी / Gõychi Konknni
#> 462                      गोंयची कोंकणी
#> 463                   Gõychi Konknni
#> 464                                 
#> 465                 Bahasa Hulontalo
#> 466                           𐌲𐌿𐍄𐌹𐍃𐌺
#> 467                  Ghanaian Pidgin
#> 468                                 
#> 469                  Ἀρχαία ἑλληνικὴ
#> 470                                 
#> 471                      Alemannisch
#> 472                                 
#> 473                           ગુજરાતી
#> 474                       wayuunaiki
#> 475                                 
#> 476                         farefare
#> 477                           gungbe
#> 478                                 
#> 479                            Gaelg
#> 480                                 
#> 481                                 
#> 482                            Hausa
#> 483                                 
#> 484                                 
#> 485                                 
#> 486                                 
#> 487                                 
#> 488              客家語 / Hak-kâ-ngî
#> 489                   客家语（简体）
#> 490                   客家語（繁體）
#> 491          Hak-kâ-ngî (Pha̍k-fa-sṳ)
#> 492                                 
#> 493                          Hawaiʻi
#> 494                                 
#> 495                                 
#> 496                                 
#> 497                            עברית
#> 498                            हिन्दी
#> 499                                 
#> 500                                 
#> 501                       Fiji Hindi
#> 502                                 
#> 503                       Fiji Hindi
#> 504                          Ilonggo
#> 505                                 
#> 506                                 
#> 507                                 
#> 508                                 
#> 509                          kihunde
#> 510                                 
#> 511                                 
#> 512                                 
#> 513                            ہندکو
#> 514                        Hiri Motu
#> 515                                 
#> 516                               Ho
#> 517                         hrvatski
#> 518                          Hunsrik
#> 519                    hornjoserbsce
#> 520                             湘語
#> 521                                 
#> 522                                 
#> 523                   Kreyòl ayisyen
#> 524                                 
#> 525                           magyar
#> 526                  magyar (formal)
#> 527                                 
#> 528                                 
#> 529                          հայերեն
#> 530                   Արեւմտահայերէն
#> 531                       Otsiherero
#> 532                      interlingua
#> 533                        Jaku Iban
#> 534                           ibibio
#> 535                 Bahasa Indonesia
#> 536                      Interlingue
#> 537                                 
#> 538                             Igbo
#> 539                                 
#> 540                            Igala
#> 541                             ꆇꉙ
#> 542                                 
#> 543                        Iñupiatun
#> 544                           ᐃᓄᒃᑎᑐᑦ
#> 545                        inuktitut
#> 546                                 
#> 547                          Ilokano
#> 548                                 
#> 549                                 
#> 550                         гӀалгӀай
#> 551                              Ido
#> 552                                 
#> 553                                 
#> 554                         íslenska
#> 555                                 
#> 556                                 
#> 557                                 
#> 558                                 
#> 559                                 
#> 560                                 
#> 561                  меджусловјанскы
#> 562                  medžuslovjansky
#> 563                         italiano
#> 564               ᐃᓄᒃᑎᑐᑦ / inuktitut
#> 565                                 
#> 566                                 
#> 567                           日本語
#> 568                                 
#> 569                                 
#> 570                                 
#> 571                                 
#> 572                                 
#> 573                                 
#> 574                           Patois
#> 575                      la .lojban.
#> 576                                 
#> 577                                 
#> 578                                 
#> 579                                 
#> 580                                 
#> 581                                 
#> 582                                 
#> 583                             jysk
#> 584                             Jawa
#> 585                               ꦗꦮ
#> 586                          ქართული
#> 587                    Qaraqalpaqsha
#> 588                        Taqbaylit
#> 589                                 
#> 590                                 
#> 591                      Karai-karai
#> 592                              Jju
#> 593                                 
#> 594                                 
#> 595                                 
#> 596                         адыгэбзэ
#> 597                         адыгэбзэ
#> 598                                 
#> 599                                 
#> 600                           Kabɩyɛ
#> 601                             Tyap
#> 602                                 
#> 603                                 
#> 604                     kabuverdianu
#> 605                                 
#> 606                                 
#> 607                                 
#> 608                                 
#> 609                                 
#> 610                            Kongo
#> 611                         Kumoring
#> 612                                 
#> 613                                 
#> 614                                 
#> 615                                 
#> 616                                 
#> 617                                 
#> 618                                 
#> 619                            کھوار
#> 620                           Gĩkũyũ
#> 621                                 
#> 622                        Kırmancki
#> 623                         Kwanyama
#> 624                            хакас
#> 625                              ဖၠုံလိက်
#> 626                          қазақша
#> 627                  قازاقشا (تٴوتە)
#> 628                  قازاقشا (جۇنگو)
#> 629                  қазақша (кирил)
#> 630              қазақша (Қазақстан)
#> 631                  qazaqşa (latın)
#> 632                qazaqşa (Türkïya)
#> 633                                 
#> 634                      kalaallisut
#> 635                                 
#> 636                                 
#> 637                                 
#> 638                                 
#> 639                                 
#> 640                         ភាសាខ្មែរ
#> 641                                 
#> 642                                 
#> 643                                 
#> 644                                 
#> 645                                 
#> 646                             ಕನ್ನಡ
#> 647                     Yerwa Kanuri
#> 648                                 
#> 649                                 
#> 650                                 
#> 651                           한국어
#> 652                                 
#> 653                                 
#> 654                                 
#> 655                           조선말
#> 656                       перем коми
#> 657                                 
#> 658                                 
#> 659                                 
#> 660                                 
#> 661                                 
#> 662                                 
#> 663                                 
#> 664                           kanuri
#> 665                 къарачай-малкъар
#> 666                             Krio
#> 667                        Kinaray-a
#> 668                           karjal
#> 669                                 
#> 670                                 
#> 671                             کٲشُر
#> 672                             کٲشُر
#> 673                             कॉशुर
#> 674                                 
#> 675                                 
#> 676                       Ripoarisch
#> 677                               စှီၤ
#> 678                                 
#> 679                            kurdî
#> 680                   کوردی (عەرەبی)
#> 681                   kurdî (latînî)
#> 682                          къумукъ
#> 683                           Kʋsaal
#> 684                                 
#> 685                             коми
#> 686                                 
#> 687                         kernowek
#> 688                                 
#> 689                                 
#> 690                                 
#> 691                                 
#> 692                                 
#> 693                         кыргызча
#> 694                                 
#> 695                                 
#> 696                           Latina
#> 697                           Ladino
#> 698                                 
#> 699                                 
#> 700                                 
#> 701                                 
#> 702                                 
#> 703                                 
#> 704                   Lëtzebuergesch
#> 705                            лакку
#> 706                                 
#> 707                                 
#> 708                                 
#> 709                            лезги
#> 710               Lingua Franca Nova
#> 711                          Luganda
#> 712                         Limburgs
#> 713                           Ligure
#> 714                                 
#> 715                                 
#> 716                         Līvõ kēļ
#> 717                      Lampung Api
#> 718                             لەکی
#> 719                                 
#> 720                            Ladin
#> 721                                 
#> 722                          lombard
#> 723                          lingála
#> 724                                 
#> 725                              ລາວ
#> 726                                 
#> 727                                 
#> 728                           Silozi
#> 729                      لۊری شومالی
#> 730                                 
#> 731                         lietuvių
#> 732                          latgaļu
#> 733                                 
#> 734                           ciluba
#> 735                                 
#> 736                                 
#> 737                                 
#> 738                                 
#> 739                       Mizo ţawng
#> 740                                 
#> 741                                 
#> 742                      لئری دوٙمینی
#> 743                         latviešu
#> 744                             文言
#> 745                           Lazuri
#> 746                          Madhurâ
#> 747                                 
#> 748                             मगही
#> 749                            मैथिली
#> 750                                 
#> 751                                 
#> 752                                 
#> 753                                 
#> 754                  Basa Banyumasan
#> 755                                 
#> 756                                 
#> 757                                 
#> 758                                 
#> 759                                 
#> 760                          мокшень
#> 761                                 
#> 762                                 
#> 763                                 
#> 764                                 
#> 765                                 
#> 766                                 
#> 767                                 
#> 768                         Malagasy
#> 769                                 
#> 770                                 
#> 771                                 
#> 772                             Ebon
#> 773                                 
#> 774                                 
#> 775                       олык марий
#> 776                            Māori
#> 777                                 
#> 778                                 
#> 779                      Minangkabau
#> 780                                 
#> 781                                 
#> 782                                 
#> 783                                 
#> 784                       македонски
#> 785                                 
#> 786                           മലയാളം
#> 787                           монгол
#> 788                                 
#> 789                                 
#> 790                      manju gisun
#> 791                      manju gisun
#> 792                      ᠮᠠᠨᠵᡠ ᡤᡳᠰᡠᠨ
#> 793                         ꯃꯤꯇꯩ ꯂꯣꯟ
#> 794                                 
#> 795                                 
#> 796                                 
#> 797                                 
#> 798                                 
#> 799                           ဘာသာမန်
#> 800                     молдовеняскэ
#> 801                                 
#> 802                                 
#> 803                            moore
#> 804                            मराठी
#> 805                                 
#> 806                             Mara
#> 807                       кырык мары
#> 808                                 
#> 809                                 
#> 810                    Bahasa Melayu
#> 811                       بهاس ملايو
#> 812                                 
#> 813                            Malti
#> 814                                 
#> 815                   Baso Palembang
#> 816                                 
#> 817                                 
#> 818                          Mvskoke
#> 819                                 
#> 820                                 
#> 821                                 
#> 822                                 
#> 823                         Mirandés
#> 824                                 
#> 825                                 
#> 826                                 
#> 827                                 
#> 828                        မြန်မာဘာသာ
#> 829                                 
#> 830                                 
#> 831                           эрзянь
#> 832                          مازِرونی
#> 833                   Dorerin Naoero
#> 834                          Nāhuatl
#> 835                                 
#> 836              閩南語 / Bân-lâm-gí
#> 837                                 
#> 838                                 
#> 839               閩南語（傳統漢字）
#> 840           Bân-lâm-gí (Pe̍h-ōe-jī)
#> 841              Bân-lâm-gí (Tâi-lô)
#> 842                       Napulitano
#> 843                                 
#> 844                     norsk bokmål
#> 845                                 
#> 846                     Plattdüütsch
#> 847                     Nedersaksies
#> 848                            नेपाली
#> 849                        नेपाल भाषा
#> 850                        Oshiwambo
#> 851                                 
#> 852                          Li Niha
#> 853                                 
#> 854                              కొలామి
#> 855                             Niuē
#> 856                                 
#> 857                       Nederlands
#> 858                                 
#> 859           Nederlands (informeel)
#> 860                                 
#> 861                                 
#> 862                            nawdm
#> 863                    norsk nynorsk
#> 864                                 
#> 865                                 
#> 866                                 
#> 867                            norsk
#> 868                            ᨣᩤᩴᨾᩮᩬᩥᨦ
#> 869                                 
#> 870                          ногайша
#> 871                                 
#> 872                                 
#> 873                           Novial
#> 874                              ߒߞߏ
#> 875              isiNdebele seSewula
#> 876                                 
#> 877                                 
#> 878                        Nouormand
#> 879                                 
#> 880                                 
#> 881                 Sesotho sa Leboa
#> 882                                 
#> 883                                 
#> 884                             Nupe
#> 885                                 
#> 886                      Diné bizaad
#> 887                                 
#> 888                                 
#> 889                        Chi-Chewa
#> 890                                 
#> 891                       runyankore
#> 892                         Orunyoro
#> 893                           Nyunga
#> 894                                 
#> 895                                 
#> 896                          occitan
#> 897                                 
#> 898                                 
#> 899                                 
#> 900                                 
#> 901                      Ojibwemowin
#> 902                                 
#> 903                                 
#> 904                                 
#> 905                                 
#> 906                                 
#> 907                                 
#> 908                                 
#> 909                    livvinkarjala
#> 910                           Oromoo
#> 911                                 
#> 912                                 
#> 913                              ଓଡ଼ିଆ
#> 914                             ирон
#> 915                                 
#> 916                                 
#> 917                                 
#> 918                                 
#> 919                                 
#> 920                                 
#> 921                                 
#> 922                                 
#> 923                                 
#> 924                            ਪੰਜਾਬੀ
#> 925                                 
#> 926                                 
#> 927                       Pangasinan
#> 928                                 
#> 929                                 
#> 930                                 
#> 931                                 
#> 932                      Kapampangan
#> 933                                 
#> 934                       Papiamentu
#> 935               Papiamento (Aruba)
#> 936                                 
#> 937                                 
#> 938                                 
#> 939                           Picard
#> 940                            Naijá
#> 941                          Deitsch
#> 942                     Plautdietsch
#> 943                                 
#> 944                         Pälzisch
#> 945                                 
#> 946                                 
#> 947                                 
#> 948                                 
#> 949                                 
#> 950                                 
#> 951                                 
#> 952                                 
#> 953                                 
#> 954                                 
#> 955                                 
#> 956                             पालि
#> 957                                 
#> 958                 Norfuk / Pitkern
#> 959                                 
#> 960                                 
#> 961                                 
#> 962                                 
#> 963                                 
#> 964                           polski
#> 965                                 
#> 966                                 
#> 967                       Piemontèis
#> 968                           پنجابی
#> 969                         Ποντιακά
#> 970                                 
#> 971                                 
#> 972                            Nawat
#> 973                                 
#> 974                                 
#> 975                                 
#> 976                                 
#> 977                        prūsiskan
#> 978                                 
#> 979                                 
#> 980                             پښتو
#> 981                                 
#> 982                                 
#> 983                                 
#> 984                                 
#> 985                                 
#> 986                                 
#> 987                                 
#> 988                                 
#> 989                                 
#> 990                        português
#> 991                                 
#> 992              português do Brasil
#> 993                                 
#> 994                                 
#> 995                       pinayuanan
#> 996                                 
#> 997                        Runa Simi
#> 998                                 
#> 999                       Runa shimi
#> 1000                                
#> 1001                                
#> 1002                                
#> 1003                                
#> 1004                                
#> 1005                                
#> 1006                                
#> 1007                                
#> 1008                                
#> 1009                                
#> 1010                                
#> 1011                        Rumagnôl
#> 1012                                
#> 1013                                
#> 1014                                
#> 1015                         Tarifit
#> 1016                             ရခိုင်
#> 1017                                
#> 1018                       rumantsch
#> 1019                                
#> 1020                                
#> 1021                                
#> 1022                                
#> 1023                                
#> 1024                                
#> 1025                     romaňi čhib
#> 1026                                
#> 1027                                
#> 1028                                
#> 1029                                
#> 1030                                
#> 1031                                
#> 1032                                
#> 1033                     romani čhib
#> 1034                        ikirundi
#> 1035                          română
#> 1036                                
#> 1037                                
#> 1038                     armãneashti
#> 1039                       tarandíne
#> 1040                                
#> 1041                                
#> 1042                           руски
#> 1043                                
#> 1044                         русский
#> 1045                                
#> 1046                      русиньскый
#> 1047                                
#> 1048                                
#> 1049                     armãneashti
#> 1050                        Vlăheşte
#> 1051                        Влахесте
#> 1052                        Vlăheşte
#> 1053                      мыхаӀбишды
#> 1054                    Ikinyarwanda
#> 1055                                
#> 1056                                
#> 1057                                
#> 1058                                
#> 1059                    うちなーぐち
#> 1060                                
#> 1061                           संस्कृतम्
#> 1062                                
#> 1063                                
#> 1064                       саха тыла
#> 1065                                
#> 1066                                
#> 1067                                
#> 1068                                
#> 1069                           Sasak
#> 1070                         ᱥᱟᱱᱛᱟᱲᱤ
#> 1071                                
#> 1072                                
#> 1073                                
#> 1074                                
#> 1075                                
#> 1076                                
#> 1077                           sardu
#> 1078                                
#> 1079                       sicilianu
#> 1080                           Scots
#> 1081                            سنڌي
#> 1082                                
#> 1083                                
#> 1084                                
#> 1085                                
#> 1086                       Sassaresu
#> 1087                     کوردی خوارگ
#> 1088                                
#> 1089                                
#> 1090                                
#> 1091                 davvisámegiella
#> 1092  davvisámegiella (Suoma bealde)
#> 1093 davvisámegiella (Norgga bealde)
#> 1094  davvisámegiella (Ruoŧa bealde)
#> 1095                                
#> 1096                                
#> 1097                                
#> 1098                     Cmique Itom
#> 1099                                
#> 1100                                
#> 1101                                
#> 1102                 Koyraboro Senni
#> 1103                           Sängö
#> 1104                                
#> 1105                                
#> 1106                                
#> 1107                                
#> 1108                                
#> 1109                                
#> 1110                      žemaitėška
#> 1111                                
#> 1112                                
#> 1113 srpskohrvatski / српскохрватски
#> 1114       српскохрватски (ћирилица)
#> 1115       srpskohrvatski (latinica)
#> 1116                                
#> 1117                         Taclḥit
#> 1118                         Taclḥit
#> 1119                         ⵜⴰⵛⵍⵃⵉⵜ
#> 1120                              တႆး
#> 1121                                
#> 1122                         tacawit
#> 1123                                
#> 1124                         tacawit
#> 1125                                
#> 1126                            සිංහල
#> 1127                                
#> 1128                                
#> 1129                  Simple English
#> 1130                                
#> 1131                                
#> 1132                 кӣллт са̄мь кӣлл
#> 1133                 bidumsámegiella
#> 1134                                
#> 1135                                
#> 1136                                
#> 1137                                
#> 1138                                
#> 1139                      slovenčina
#> 1140                         سرائیکی
#> 1141                         سرائیکی
#> 1142                     slovenščina
#> 1143                                
#> 1144                                
#> 1145                        Schläsch
#> 1146                                
#> 1147                                
#> 1148                    Gagana Samoa
#> 1149                   åarjelsaemien
#> 1150                                
#> 1151                                
#> 1152                     anarâškielâ
#> 1153                nuõrttsääʹmǩiõll
#> 1154                        chiShona
#> 1155                                
#> 1156                                
#> 1157                      Soomaaliga
#> 1158                                
#> 1159                                
#> 1160                                
#> 1161                           shqip
#> 1162                 српски / srpski
#> 1163               српски (ћирилица)
#> 1164               српски (ћирилица)
#> 1165               srpski (latinica)
#> 1166               srpski (latinica)
#> 1167                                
#> 1168                                
#> 1169                                
#> 1170                                
#> 1171                                
#> 1172                     Sranantongo
#> 1173               sardu campidanesu
#> 1174                                
#> 1175                                
#> 1176                         SiSwati
#> 1177                                
#> 1178                                
#> 1179                                
#> 1180                                
#> 1181                         Sesotho
#> 1182                                
#> 1183                       Seeltersk
#> 1184                                
#> 1185                      себертатар
#> 1186                           Sunda
#> 1187                                
#> 1188                                
#> 1189                                
#> 1190                                
#> 1191                                
#> 1192                                
#> 1193                         svenska
#> 1194                                
#> 1195                       Kiswahili
#> 1196                                
#> 1197                                
#> 1198                                
#> 1199                                
#> 1200                                
#> 1201                           ꠍꠤꠟꠐꠤ
#> 1202                                
#> 1203                                
#> 1204                                
#> 1205                         ślůnski
#> 1206                        Sakizaya
#> 1207                            தமிழ்
#> 1208                                
#> 1209                                
#> 1210                           Tayal
#> 1211                                
#> 1212                                
#> 1213                            ತುಳು
#> 1214                    ᥖᥭᥰ ᥖᥬᥲ ᥑᥨᥒᥰ
#> 1215                           తెలుగు
#> 1216                                
#> 1217                                
#> 1218                                
#> 1219                           tetun
#> 1220                          тоҷикӣ
#> 1221                          тоҷикӣ
#> 1222                          tojikī
#> 1223                                
#> 1224                             ไทย
#> 1225                                
#> 1226                                
#> 1227                                
#> 1228                            ትግርኛ
#> 1229                             ትግሬ
#> 1230                                
#> 1231                                
#> 1232                                
#> 1233                       Türkmençe
#> 1234                                
#> 1235                                
#> 1236                         Tagalog
#> 1237                                
#> 1238                                
#> 1239                                
#> 1240                                
#> 1241                                
#> 1242                          tolışi
#> 1243                          толыши
#> 1244                                
#> 1245                                
#> 1246                        Setswana
#> 1247                                
#> 1248                  lea faka-Tonga
#> 1249                                
#> 1250                                
#> 1251                       toki pona
#> 1252                       Tok Pisin
#> 1253                          Türkçe
#> 1254                                
#> 1255                          Ṫuroyo
#> 1256                          Seediq
#> 1257                                
#> 1258                        Xitsonga
#> 1259                                
#> 1260                                
#> 1261                                
#> 1262                                
#> 1263               татарча / tatarça
#> 1264                         татарча
#> 1265                         tatarça
#> 1266                        Orutooro
#> 1267                                
#> 1268                                
#> 1269                                
#> 1270                      chiTumbuka
#> 1271                                
#> 1272                                
#> 1273                                
#> 1274                                
#> 1275                             Twi
#> 1276                                
#> 1277                                
#> 1278                                
#> 1279                                
#> 1280                                
#> 1281                                
#> 1282                                
#> 1283                      reo tahiti
#> 1284                        тыва дыл
#> 1285                                
#> 1286                        ⵜⴰⵎⴰⵣⵉⵖⵜ
#> 1287                          удмурт
#> 1288            ئۇيغۇرچە / Uyghurche
#> 1289                        ئۇيغۇرچە
#> 1290                                
#> 1291                       Uyghurche
#> 1292                                
#> 1293                      українська
#> 1294                                
#> 1295                                
#> 1296                                
#> 1297                                
#> 1298                                
#> 1299                                
#> 1300                                
#> 1301                                
#> 1302                            اردو
#> 1303                                
#> 1304                                
#> 1305                                
#> 1306             oʻzbekcha / ўзбекча
#> 1307                         ўзбекча
#> 1308                       oʻzbekcha
#> 1309                                
#> 1310                       Tshivenda
#> 1311                          vèneto
#> 1312                     vepsän kel’
#> 1313                      Tiếng Việt
#> 1314                                
#> 1315                      West-Vlams
#> 1316                   Mainfränkisch
#> 1317                        emakhuwa
#> 1318                         Volapük
#> 1319                           Vaďďa
#> 1320                            võro
#> 1321                                
#> 1322                                
#> 1323                           walon
#> 1324                                
#> 1325                                
#> 1326                        wolaytta
#> 1327                         Winaray
#> 1328                                
#> 1329                                
#> 1330                                
#> 1331                                
#> 1332                                
#> 1333                                
#> 1334                                
#> 1335                                
#> 1336                                
#> 1337                                
#> 1338                                
#> 1339                       Fakaʻuvea
#> 1340                           waale
#> 1341                           Wolof
#> 1342                                
#> 1343                                
#> 1344                            吴语
#> 1345                    吴语（简体）
#> 1346                    吳語（正體）
#> 1347                                
#> 1348                                
#> 1349                          хальмг
#> 1350                                
#> 1351                        isiXhosa
#> 1352                       მარგალური
#> 1353                                
#> 1354                                
#> 1355                                
#> 1356                                
#> 1357                                
#> 1358                                
#> 1359                                
#> 1360                                
#> 1361                                
#> 1362                                
#> 1363                        saisiyat
#> 1364                                
#> 1365                                
#> 1366                                
#> 1367                                
#> 1368                                
#> 1369                                
#> 1370                                
#> 1371                                
#> 1372                                
#> 1373                                
#> 1374                                
#> 1375                                
#> 1376                                
#> 1377                           ייִדיש
#> 1378                                
#> 1379                          Yorùbá
#> 1380                                
#> 1381                                
#> 1382                                
#> 1383                                
#> 1384                                
#> 1385                                
#> 1386                        Nhẽẽgatú
#> 1387                     maaya t’aan
#> 1388                            粵語
#> 1389                    粵语（简体）
#> 1390                    粵語（繁體）
#> 1391                       Vahcuengh
#> 1392                                
#> 1393                                
#> 1394                                
#> 1395                          Zeêuws
#> 1396                                
#> 1397               ⵜⴰⵎⴰⵣⵉⵖⵜ ⵜⴰⵏⴰⵡⴰⵢⵜ
#> 1398               tamaziɣt tanawayt
#> 1399                            中文
#> 1400                            文言
#> 1401                中文（中国大陆）
#> 1402                    中文（简体）
#> 1403                    中文（繁體）
#> 1404                    中文（香港）
#> 1405             閩南語 / Bân-lâm-gí
#> 1406                    中文（澳門）
#> 1407                中文（马来西亚）
#> 1408                  中文（新加坡）
#> 1409                    中文（臺灣）
#> 1410                            粵語
#> 1411                                
#> 1412                                
#> 1413                                
#> 1414                         isiZulu
#> 1415                                
#> 1416                                
#> 1417                                
```
