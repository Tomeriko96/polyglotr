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
#>              language_tag
#> 1                      aa
#> 2                     aae
#> 3                      ab
#> 4                     abe
#> 5                     abq
#> 6                abq-latn
#> 7                     abr
#> 8                     abs
#> 9                     abu
#> 10                    ace
#> 11                    acf
#> 12                    ach
#> 13                    acm
#> 14                    ada
#> 15                    adg
#> 16                    adj
#> 17                    ady
#> 18               ady-cyrl
#> 19               ady-latn
#> 20                     ae
#> 21                    aeb
#> 22               aeb-arab
#> 23               aeb-latn
#> 24                    aec
#> 25                    aee
#> 26                    aer
#> 27                     af
#> 28                    afa
#> 29                    afh
#> 30                    agq
#> 31                    aha
#> 32                    ahr
#> 33                    aig
#> 34                    aii
#> 35                    ain
#> 36                    ajg
#> 37                    ajp
#> 38               ajp-arab
#> 39               ajp-latn
#> 40                     ak
#> 41                    akb
#> 42                    akk
#> 43               akk-latn
#> 44               akk-xsux
#> 45                    akz
#> 46                    alc
#> 47                    ale
#> 48               ale-cyrl
#> 49                    alg
#> 50                    aln
#> 51                    alq
#> 52                    als
#> 53                    alt
#> 54                    aly
#> 55                     am
#> 56                    ami
#> 57                    amx
#> 58                     an
#> 59                    ane
#> 60                    ang
#> 61                    ann
#> 62                    anp
#> 63                    apa
#> 64                    apc
#> 65               apc-arab
#> 66               apc-latn
#> 67                    apw
#> 68                     ar
#> 69                 ar-001
#> 70                    arc
#> 71                    are
#> 72                    arn
#> 73                    aro
#> 74                    arp
#> 75                    arq
#> 76                    ars
#> 77                    art
#> 78                    arw
#> 79                    ary
#> 80               ary-arab
#> 81               ary-latn
#> 82                    arz
#> 83                     as
#> 84                    asa
#> 85                    ase
#> 86                    ast
#> 87                    ath
#> 88                    ati
#> 89                    atj
#> 90                    atv
#> 91                    aus
#> 92                     av
#> 93                    avi
#> 94                    avk
#> 95                    awa
#> 96                    axe
#> 97                    axl
#> 98                     ay
#> 99                    ayh
#> 100                   ayz
#> 101                    az
#> 102               az-arab
#> 103               az-cyrl
#> 104               az-latn
#> 105                   azb
#> 106                   azj
#> 107                    ba
#> 108                   bad
#> 109                   bag
#> 110                   bai
#> 111                   bal
#> 112              bal-latn
#> 113                   ban
#> 114              ban-bali
#> 115                   bar
#> 116                   bas
#> 117                   bat
#> 118               bat-smg
#> 119                   bax
#> 120                   bbc
#> 121              bbc-batk
#> 122              bbc-latn
#> 123                   bbj
#> 124                   bcc
#> 125                   bci
#> 126                   bcl
#> 127                   bdr
#> 128                    be
#> 129             be-tarask
#> 130              be-x-old
#> 131                   bej
#> 132                   bem
#> 133                   ber
#> 134                   bew
#> 135                   bez
#> 136                   bfa
#> 137                   bfd
#> 138                   bfi
#> 139                   bfq
#> 140                   bft
#> 141              bft-tibt
#> 142                   bfw
#> 143                   bfz
#> 144              bfz-deva
#> 145              bfz-takr
#> 146                    bg
#> 147                   bgc
#> 148              bgc-arab
#> 149              bgc-deva
#> 150                   bgn
#> 151                   bgp
#> 152                   bgq
#> 153              bgq-arab
#> 154              bgq-deva
#> 155                    bh
#> 156                   bha
#> 157                   bhd
#> 158              bhd-deva
#> 159              bhd-takr
#> 160                   bho
#> 161                    bi
#> 162                   bik
#> 163                   bin
#> 164                   bjn
#> 165                   bkc
#> 166                   bkh
#> 167                   bkm
#> 168                   bkn
#> 169                   bla
#> 170                   blc
#> 171                   blk
#> 172                   blo
#> 173                   blt
#> 174                    bm
#> 175                    bn
#> 176               bn-sylo
#> 177                   bnb
#> 178                   bnn
#> 179                   bnt
#> 180                   bny
#> 181                    bo
#> 182                   bol
#> 183                   bom
#> 184                   bpy
#> 185                   bqi
#> 186                   bqz
#> 187                    br
#> 188                   bra
#> 189                   brh
#> 190              brh-latn
#> 191                   brx
#> 192                    bs
#> 193                   bse
#> 194                   bsk
#> 195                   bss
#> 196                   btd
#> 197                   bth
#> 198                   btk
#> 199                   btm
#> 200                   bto
#> 201                   bts
#> 202                   btx
#> 203                   btz
#> 204                   bua
#> 205                   bug
#> 206              bug-bugi
#> 207                   bum
#> 208                   bvb
#> 209                   bwr
#> 210                   bxr
#> 211                   byn
#> 212                   byv
#> 213                   bzj
#> 214                   bzs
#> 215                    ca
#> 216                   cad
#> 217                   cai
#> 218                   cak
#> 219                   cal
#> 220                   car
#> 221                   cau
#> 222                   cay
#> 223                   cbk
#> 224               cbk-zam
#> 225                   cch
#> 226                   ccp
#> 227              ccp-beng
#> 228                   cdo
#> 229              cdo-hani
#> 230              cdo-hant
#> 231              cdo-latn
#> 232              cdz-beng
#> 233                    ce
#> 234                   ceb
#> 235                   cel
#> 236                   cgg
#> 237                    ch
#> 238                   chb
#> 239                   chg
#> 240                   chk
#> 241                   chm
#> 242                   chn
#> 243                   cho
#> 244                   chp
#> 245                   chr
#> 246                   chy
#> 247                   cic
#> 248                   ciw
#> 249                   cja
#> 250              cja-arab
#> 251              cja-cham
#> 252              cja-latn
#> 253                   cjm
#> 254              cjm-arab
#> 255              cjm-cham
#> 256              cjm-latn
#> 257                   cjy
#> 258              cjy-hans
#> 259              cjy-hant
#> 260                   ckb
#> 261              ckb-arab
#> 262              ckb-latn
#> 263                   cko
#> 264                   ckt
#> 265                   ckv
#> 266                   clc
#> 267                   cmc
#> 268                   cmg
#> 269    cmn-latn-cn-pinyin
#> 270    cmn-latn-tw-pinyin
#> 271  cmn-latn-tw-tongyong
#> 272  cmn-latn-tw-wadegile
#> 273                   cnh
#> 274                   cnr
#> 275              cnr-cyrl
#> 276              cnr-latn
#> 277                   cnx
#> 278                    co
#> 279                   coa
#> 280                   cop
#> 281                   cpe
#> 282                   cpf
#> 283                   cpp
#> 284                   cps
#> 285                   cpx
#> 286              cpx-hans
#> 287              cpx-hant
#> 288              cpx-latn
#> 289                    cr
#> 290               cr-cans
#> 291               cr-latn
#> 292                   crb
#> 293                   crg
#> 294                   crh
#> 295              crh-cyrl
#> 296              crh-latn
#> 297                crh-ro
#> 298                   crj
#> 299                   crk
#> 300                   crl
#> 301                   crm
#> 302                   crp
#> 303                   crr
#> 304                   crs
#> 305                    cs
#> 306                   csb
#> 307                   csw
#> 308                   ctg
#> 309                    cu
#> 310                   cus
#> 311                    cv
#> 312                    cy
#> 313                    da
#> 314                   dag
#> 315                   dak
#> 316                   dar
#> 317                   dav
#> 318                   day
#> 319                   dbj
#> 320                   ddn
#> 321                    de
#> 322               de-1901
#> 323                 de-at
#> 324                 de-ch
#> 325             de-formal
#> 326                   del
#> 327                   den
#> 328                   dga
#> 329                   dgr
#> 330                   din
#> 331                   diq
#> 332                   dje
#> 333                   djk
#> 334                   dkr
#> 335                   dlg
#> 336                   dmg
#> 337                   dmv
#> 338                   doi
#> 339              doi-arab
#> 340              doi-deva
#> 341              doi-dogr
#> 342                   dpp
#> 343                   dra
#> 344                   drg
#> 345                   dro
#> 346                   dru
#> 347                   dsb
#> 348                   dso
#> 349                   dtb
#> 350                   dtp
#> 351                   dtr
#> 352                   dty
#> 353                   dua
#> 354                   duf
#> 355                   dum
#> 356                    dv
#> 357                   dyo
#> 358                   dyu
#> 359                    dz
#> 360                   dzg
#> 361                   ebu
#> 362                    ee
#> 363                   efi
#> 364                   egl
#> 365                   egy
#> 366                   eka
#> 367                   ekp
#> 368                    el
#> 369                 el-cy
#> 370                   elm
#> 371                   elx
#> 372                   eme
#> 373                   eml
#> 374                    en
#> 375                 en-au
#> 376                 en-ca
#> 377               en-dsrt
#> 378            en-emodeng
#> 379                 en-gb
#> 380                 en-in
#> 381                 en-jm
#> 382                 en-nz
#> 383               en-shaw
#> 384             en-simple
#> 385                 en-uk
#> 386                 en-us
#> 387                   enm
#> 388                    eo
#> 389           eo-hsistemo
#> 390               eo-shaw
#> 391           eo-xsistemo
#> 392                    es
#> 393                es-419
#> 394                 es-es
#> 395             es-formal
#> 396                 es-mx
#> 397                 es-ni
#> 398                   ess
#> 399                   esu
#> 400                    et
#> 401                   eto
#> 402                   ett
#> 403                   etu
#> 404                    eu
#> 405                   ewo
#> 406                   ext
#> 407                   eya
#> 408                    fa
#> 409                fa-034
#> 410                 fa-af
#> 411                   fab
#> 412                   fan
#> 413                   fat
#> 414                   fax
#> 415                   fay
#> 416                    ff
#> 417                    fi
#> 418                   fil
#> 419                   fit
#> 420                   fiu
#> 421               fiu-vro
#> 422                    fj
#> 423                   fkv
#> 424                   fmp
#> 425                    fo
#> 426                   fon
#> 427                   fos
#> 428                    fr
#> 429                 fr-be
#> 430                 fr-ca
#> 431                 fr-ch
#> 432                   frc
#> 433                   frk
#> 434                   frm
#> 435                   fro
#> 436                   frp
#> 437                   frr
#> 438                   frs
#> 439                   fsl
#> 440                   fud
#> 441                   fuf
#> 442                   fur
#> 443                   fvr
#> 444                    fy
#> 445                    ga
#> 446                   gaa
#> 447                   gag
#> 448                   gah
#> 449                   gan
#> 450              gan-hans
#> 451              gan-hant
#> 452                   gay
#> 453                   gba
#> 454                   gbb
#> 455                   gbk
#> 456              gbk-deva
#> 457              gbk-takr
#> 458                   gbm
#> 459                   gbz
#> 460                   gcf
#> 461                   gcr
#> 462                    gd
#> 463                   gem
#> 464                   gez
#> 465                   gil
#> 466                   gju
#> 467              gju-arab
#> 468              gju-deva
#> 469                    gl
#> 470                   gld
#> 471                   glh
#> 472                   glk
#> 473                   gmh
#> 474                   gml
#> 475                   gmy
#> 476                    gn
#> 477                   gnq
#> 478                   goh
#> 479                   gom
#> 480              gom-deva
#> 481              gom-latn
#> 482                   gon
#> 483                   gor
#> 484                   got
#> 485                   gpe
#> 486                   grb
#> 487                   grc
#> 488                   gsg
#> 489                   gsw
#> 490                gsw-fr
#> 491                    gu
#> 492                   guc
#> 493                   gum
#> 494                   gur
#> 495                   guw
#> 496                   guz
#> 497                    gv
#> 498                   gwi
#> 499                   gya
#> 500                    ha
#> 501               ha-arab
#> 502               ha-latn
#> 503                 ha-ne
#> 504                   hac
#> 505                   hai
#> 506                   hak
#> 507              hak-hans
#> 508              hak-hant
#> 509              hak-latn
#> 510                   hav
#> 511                   haw
#> 512                   hax
#> 513                   haz
#> 514                   hbo
#> 515                    he
#> 516                   hea
#> 517                    hi
#> 518               hi-kthi
#> 519               hi-latn
#> 520                   hif
#> 521              hif-deva
#> 522              hif-latn
#> 523                   hil
#> 524                   him
#> 525                   hit
#> 526              hit-latn
#> 527              hit-xsux
#> 528                   hke
#> 529                   hmn
#> 530                   hne
#> 531                   hnj
#> 532                   hno
#> 533                    ho
#> 534                   hoc
#> 535              hoc-latn
#> 536                    hr
#> 537               hr-glag
#> 538                   hrx
#> 539                   hsb
#> 540                   hsn
#> 541              hsn-hans
#> 542              hsn-hant
#> 543                    ht
#> 544                   hts
#> 545                    hu
#> 546             hu-formal
#> 547                   hup
#> 548                   hur
#> 549                    hy
#> 550                   hyw
#> 551                    hz
#> 552                    ia
#> 553                   iba
#> 554                   ibb
#> 555                    id
#> 556                    ie
#> 557                   ifu
#> 558                    ig
#> 559                   igb
#> 560                   igl
#> 561                    ii
#> 562                   ijo
#> 563                    ik
#> 564              ike-cans
#> 565              ike-latn
#> 566                   ikt
#> 567                   ilo
#> 568                   inc
#> 569                   ine
#> 570                   inh
#> 571                    io
#> 572                   ira
#> 573                   iro
#> 574                    is
#> 575                   ish
#> 576              isk-arab
#> 577              isk-cyrl
#> 578              isk-latn
#> 579                   ist
#> 580                   isu
#> 581                   isv
#> 582              isv-cyrl
#> 583              isv-latn
#> 584                    it
#> 585                    iu
#> 586                   ivb
#> 587                   izh
#> 588                   izr
#> 589                    ja
#> 590               ja-hani
#> 591               ja-hira
#> 592               ja-hrkt
#> 593               ja-kana
#> 594                   jac
#> 595                   jak
#> 596                   jam
#> 597                   jax
#> 598                   jbo
#> 599                   jdt
#> 600              jdt-cyrl
#> 601                   jgo
#> 602                   jje
#> 603                   jmc
#> 604                   jpr
#> 605                   jrb
#> 606                   juk
#> 607                   jut
#> 608                    jv
#> 609               jv-java
#> 610                    ka
#> 611                   kaa
#> 612                   kab
#> 613                   kac
#> 614                   kag
#> 615                   kai
#> 616                   kaj
#> 617                   kam
#> 618                   kar
#> 619                   kaw
#> 620                   kbd
#> 621              kbd-cyrl
#> 622              kbd-latn
#> 623                   kbl
#> 624                   kbp
#> 625                   kcg
#> 626                   kck
#> 627                   kde
#> 628                   kea
#> 629                   kek
#> 630                   ken
#> 631                   ker
#> 632                   kfo
#> 633                   kfr
#> 634                    kg
#> 635                   kge
#> 636              kge-arab
#> 637                   kgg
#> 638                   kgp
#> 639                   kha
#> 640                   khi
#> 641                   kho
#> 642                   khq
#> 643                   khw
#> 644                    ki
#> 645                   kip
#> 646                   kiu
#> 647                   kix
#> 648                    kj
#> 649                   kjh
#> 650                   kjp
#> 651                    kk
#> 652               kk-arab
#> 653                 kk-cn
#> 654               kk-cyrl
#> 655                 kk-kz
#> 656               kk-latn
#> 657                 kk-tr
#> 658                   kkj
#> 659                    kl
#> 660                   kld
#> 661                   kln
#> 662                   kls
#> 663              kls-arab
#> 664              kls-latn
#> 665                    km
#> 666                   kmb
#> 667                   kmr
#> 668              kmr-arab
#> 669              kmr-latn
#> 670                   kmz
#> 671                    kn
#> 672                   knc
#> 673                   kne
#> 674                   knn
#> 675                   knq
#> 676                    ko
#> 677                 ko-cn
#> 678               ko-hani
#> 679               ko-kore
#> 680                 ko-kp
#> 681                 ko-kr
#> 682                   koi
#> 683                   kok
#> 684                   kos
#> 685                   koy
#> 686                   kpe
#> 687                   kqr
#> 688                   kqt
#> 689                   kqv
#> 690                    kr
#> 691                   krc
#> 692                   kri
#> 693                   krj
#> 694                   krl
#> 695                   kro
#> 696                   kru
#> 697                    ks
#> 698               ks-arab
#> 699               ks-deva
#> 700                   ksb
#> 701                   ksf
#> 702                   ksh
#> 703                   ksw
#> 704              ksy-beng
#> 705                    ku
#> 706               ku-arab
#> 707               ku-latn
#> 708                   kum
#> 709                   kus
#> 710                   kut
#> 711                    kv
#> 712                   kve
#> 713                    kw
#> 714                   kwk
#> 715                   kxd
#> 716                   kxi
#> 717                   kxn
#> 718                   kxv
#> 719                    ky
#> 720              kyw-beng
#> 721              kyw-deva
#> 722                    la
#> 723                   lad
#> 724              lad-hebr
#> 725              lad-latn
#> 726                   lag
#> 727                   lah
#> 728                   laj
#> 729                   lam
#> 730                    lb
#> 731                   lbe
#> 732                   lcm
#> 733                   ldn
#> 734                   lem
#> 735                   lez
#> 736                   lfn
#> 737                    lg
#> 738                    li
#> 739                 li-be
#> 740                 li-nl
#> 741                   lij
#> 742                lij-mc
#> 743                   lil
#> 744                   liv
#> 745                   ljp
#> 746                   lki
#> 747                   lkt
#> 748                   lld
#> 749                   lmn
#> 750              lmn-deva
#> 751              lmn-knda
#> 752              lmn-taml
#> 753              lmn-telu
#> 754                   lmo
#> 755                    ln
#> 756                   lns
#> 757                    lo
#> 758                   lol
#> 759                   lom
#> 760                   lou
#> 761                   loz
#> 762                   lrc
#> 763                   lsm
#> 764                    lt
#> 765                   ltg
#> 766                    lu
#> 767                   lua
#> 768                   lud
#> 769                   lui
#> 770                   lun
#> 771                   luo
#> 772                   lus
#> 773                   lut
#> 774                   luy
#> 775                   luz
#> 776                    lv
#> 777                   lzh
#> 778                   lzz
#> 779                   mad
#> 780                   maf
#> 781                   mag
#> 782                   mai
#> 783                   mak
#> 784              mak-bugi
#> 785                   man
#> 786                   map
#> 787               map-bms
#> 788                   mas
#> 789                   maw
#> 790                   mcn
#> 791                   mcp
#> 792                   mde
#> 793                   mdf
#> 794                   mdh
#> 795                   mdr
#> 796                   men
#> 797                   mer
#> 798                   mey
#> 799                   mfa
#> 800                   mfe
#> 801                    mg
#> 802                   mga
#> 803                   mgh
#> 804                   mgo
#> 805                    mh
#> 806                   mhk
#> 807                   mhn
#> 808                   mhr
#> 809                    mi
#> 810                   mic
#> 811                   mid
#> 812                   min
#> 813                   miq
#> 814                   mis
#> 815                   mix
#> 816                   mjd
#> 817              mjx-beng
#> 818                    mk
#> 819                   mkh
#> 820                    ml
#> 821                    mn
#> 822               mn-cyrl
#> 823               mn-mong
#> 824                   mnc
#> 825              mnc-latn
#> 826              mnc-mong
#> 827                   mni
#> 828              mni-beng
#> 829                   mnj
#> 830                   mno
#> 831                   mnq
#> 832                   mns
#> 833                   mnw
#> 834                    mo
#> 835                   moe
#> 836                   moh
#> 837                   mos
#> 838                    mr
#> 839               mr-modi
#> 840                   mrh
#> 841                   mrj
#> 842                   mrt
#> 843                   mrv
#> 844                    ms
#> 845               ms-arab
#> 846                   msi
#> 847                    mt
#> 848                   mua
#> 849                   mui
#> 850                   mul
#> 851                   mun
#> 852                   mus
#> 853                   mvf
#> 854                   mvi
#> 855              mvi-hira
#> 856                   mvv
#> 857                   mwl
#> 858                   mwr
#> 859                   mwv
#> 860                   mww
#> 861              mww-latn
#> 862                    my
#> 863                   mye
#> 864                   myn
#> 865                   myv
#> 866                   mzn
#> 867                    na
#> 868                   nah
#> 869                   nai
#> 870                   nan
#> 871              nan-hani
#> 872              nan-hans
#> 873              nan-hant
#> 874      nan-latn-pehoeji
#> 875        nan-latn-tailo
#> 876                   nap
#> 877                   naq
#> 878                    nb
#> 879                    nd
#> 880                   nds
#> 881                nds-nl
#> 882                    ne
#> 883                   new
#> 884                    ng
#> 885                   nge
#> 886                   nia
#> 887                   nic
#> 888                   nit
#> 889                   niu
#> 890                   njo
#> 891                    nl
#> 892                 nl-aw
#> 893                 nl-be
#> 894                 nl-cw
#> 895           nl-informal
#> 896                 nl-nl
#> 897                 nl-sr
#> 898                 nl-sx
#> 899         nl-u-sd-bebru
#> 900                   nla
#> 901                   nmg
#> 902                   nmz
#> 903                    nn
#> 904           nn-hognorsk
#> 905                   nnh
#> 906                   nnz
#> 907                    no
#> 908                   nod
#> 909              nod-thai
#> 910                   nog
#> 911                   non
#> 912              non-runr
#> 913                   nov
#> 914                   nqo
#> 915                    nr
#> 916                nrf-gg
#> 917                nrf-je
#> 918                   nrm
#> 919                   nsk
#> 920                   nsl
#> 921                   nso
#> 922                   ntd
#> 923                   nub
#> 924                   nup
#> 925                   nus
#> 926                    nv
#> 927                   nwc
#> 928                   nxm
#> 929                    ny
#> 930                   nym
#> 931                   nyn
#> 932                   nyo
#> 933                   nys
#> 934                   nzi
#> 935                   obt
#> 936                    oc
#> 937                   oco
#> 938                   odt
#> 939                   ofs
#> 940                    oj
#> 941                   ojb
#> 942                   ojc
#> 943                   ojp
#> 944              ojp-hani
#> 945              ojp-hira
#> 946                   ojs
#> 947                   ojw
#> 948                   oka
#> 949                   olo
#> 950                    om
#> 951                   oma
#> 952                   onw
#> 953                   ood
#> 954                    or
#> 955                    os
#> 956                   osa
#> 957              osa-latn
#> 958                   osi
#> 959                   osx
#> 960                   ota
#> 961                   otk
#> 962                   oto
#> 963                   ovd
#> 964                   owl
#> 965                   oym
#> 966                    pa
#> 967               pa-guru
#> 968                   paa
#> 969                   pag
#> 970                   pal
#> 971              pal-phli
#> 972              pal-phlp
#> 973              pal-phlv
#> 974                   pam
#> 975                   pao
#> 976                   pap
#> 977                pap-aw
#> 978                   paq
#> 979                   pau
#> 980                   pbb
#> 981                   pcd
#> 982                pcd-be
#> 983                pcd-fr
#> 984                   pcm
#> 985                   pdc
#> 986                   pdt
#> 987                   peo
#> 988                   pfl
#> 989                   pgd
#> 990              pgd-arab
#> 991              pgd-deva
#> 992              pgd-khar
#> 993                   pgl
#> 994                   phi
#> 995                   phl
#> 996                   phn
#> 997              phn-latn
#> 998              phn-phnx
#> 999                   phr
#> 1000                   pi
#> 1001              pi-sidd
#> 1002                  pih
#> 1003                  pis
#> 1004                  pjt
#> 1005                  pkc
#> 1006                  pko
#> 1007                  pks
#> 1008                   pl
#> 1009                  plu
#> 1010                  plv
#> 1011                  plw
#> 1012                  pms
#> 1013                  pnb
#> 1014                  pnt
#> 1015                  pon
#> 1016                  pov
#> 1017                  ppl
#> 1018                  ppu
#> 1019                  pqm
#> 1020                  pra
#> 1021                  prc
#> 1022                  prg
#> 1023                  pro
#> 1024                  prs
#> 1025                   ps
#> 1026                ps-af
#> 1027                ps-pk
#> 1028                  psh
#> 1029                  psi
#> 1030                  psu
#> 1031             psu-arab
#> 1032             psu-brah
#> 1033             psu-deva
#> 1034             psu-guru
#> 1035                   pt
#> 1036            pt-ao1990
#> 1037                pt-br
#> 1038          pt-colb1945
#> 1039                pt-pt
#> 1040                  pwn
#> 1041                  pwo
#> 1042                  pyu
#> 1043                  pzh
#> 1044                   qu
#> 1045                  quc
#> 1046                  qug
#> 1047                  qwh
#> 1048                  qxp
#> 1049                  qxq
#> 1050                  qya
#> 1051                  rag
#> 1052                  rah
#> 1053                  raj
#> 1054                  rap
#> 1055                  rar
#> 1056                  rcf
#> 1057                  rej
#> 1058                  rgn
#> 1059                  rhg
#> 1060             rhg-arab
#> 1061             rhg-rohg
#> 1062                  rif
#> 1063                  rji
#> 1064                  rki
#> 1065                  rkt
#> 1066                   rm
#> 1067             rm-puter
#> 1068             rm-rumgr
#> 1069          rm-surmiran
#> 1070           rm-sursilv
#> 1071           rm-sutsilv
#> 1072          rm-vallader
#> 1073                  rmc
#> 1074                  rmf
#> 1075                  rmg
#> 1076                  rml
#> 1077             rml-cyrl
#> 1078                  rmn
#> 1079                  rmo
#> 1080                  rmw
#> 1081                  rmy
#> 1082                   rn
#> 1083                  rnp
#> 1084                   ro
#> 1085                ro-md
#> 1086                  roa
#> 1087              roa-rup
#> 1088             roa-tara
#> 1089                  rof
#> 1090                  rom
#> 1091                  rsk
#> 1092                  rtm
#> 1093                   ru
#> 1094          ru-petr1708
#> 1095                  rue
#> 1096                  rug
#> 1097                  ruo
#> 1098                  rup
#> 1099                  ruq
#> 1100             ruq-cyrl
#> 1101             ruq-latn
#> 1102                  rut
#> 1103                   rw
#> 1104                  rwk
#> 1105                  rwr
#> 1106                  rys
#> 1107             rys-hira
#> 1108                  ryu
#> 1109             ryu-hira
#> 1110                   sa
#> 1111              sa-sidd
#> 1112                  sad
#> 1113                  sah
#> 1114                  sai
#> 1115                  sal
#> 1116                  sam
#> 1117                  saq
#> 1118                  sas
#> 1119                  sat
#> 1120             sat-beng
#> 1121             sat-latn
#> 1122             sat-orya
#> 1123                  saz
#> 1124                  sba
#> 1125                  sbp
#> 1126                   sc
#> 1127                  sci
#> 1128                  scl
#> 1129                  scn
#> 1130                  sco
#> 1131                  scz
#> 1132                   sd
#> 1133              sd-deva
#> 1134              sd-gujr
#> 1135              sd-khoj
#> 1136              sd-sind
#> 1137                  sdc
#> 1138                  sdh
#> 1139             sdh-arab
#> 1140             sdh-latn
#> 1141                  sdo
#> 1142                   se
#> 1143                se-fi
#> 1144                se-no
#> 1145                se-se
#> 1146                  sea
#> 1147                  see
#> 1148                  seh
#> 1149                  sei
#> 1150                  sel
#> 1151                  sem
#> 1152                  ser
#> 1153                  ses
#> 1154                  sfb
#> 1155                   sg
#> 1156                  sga
#> 1157                  sgh
#> 1158             sgh-arab
#> 1159             sgh-cyrl
#> 1160             sgh-latn
#> 1161                  sgn
#> 1162                  sgs
#> 1163             sgy-arab
#> 1164             sgy-latn
#> 1165                   sh
#> 1166              sh-cyrl
#> 1167              sh-latn
#> 1168                  shd
#> 1169                  shi
#> 1170             shi-latn
#> 1171             shi-tfng
#> 1172                  shn
#> 1173                  shu
#> 1174                  shy
#> 1175             shy-arab
#> 1176             shy-latn
#> 1177             shy-tfng
#> 1178                   si
#> 1179                  sia
#> 1180                  sid
#> 1181               simple
#> 1182                  sio
#> 1183                  sit
#> 1184                  sjd
#> 1185                  sje
#> 1186                  sjk
#> 1187                  sjn
#> 1188                  sjo
#> 1189                  sjs
#> 1190                  sjt
#> 1191                  sju
#> 1192                   sk
#> 1193                  skr
#> 1194             skr-arab
#> 1195                   sl
#> 1196                  sla
#> 1197                  slh
#> 1198                  sli
#> 1199                  slr
#> 1200                  sly
#> 1201                   sm
#> 1202                  sma
#> 1203                  smi
#> 1204                  smj
#> 1205                  smn
#> 1206                  sms
#> 1207                   sn
#> 1208                  sne
#> 1209                  snk
#> 1210                   so
#> 1211                  sog
#> 1212                  son
#> 1213                  spv
#> 1214                   sq
#> 1215                   sr
#> 1216              sr-cyrl
#> 1217                sr-ec
#> 1218                sr-el
#> 1219              sr-latn
#> 1220                sr-me
#> 1221             srh-arab
#> 1222             srh-cyrl
#> 1223             srh-latn
#> 1224                  srk
#> 1225                  srn
#> 1226                  sro
#> 1227                  srq
#> 1228                  srr
#> 1229                   ss
#> 1230                  ssa
#> 1231                  ssb
#> 1232                  ssf
#> 1233                  ssy
#> 1234                   st
#> 1235                  sth
#> 1236                  stq
#> 1237                  str
#> 1238                  sty
#> 1239                   su
#> 1240                  suk
#> 1241                  sus
#> 1242                  sux
#> 1243             sux-latn
#> 1244             sux-xsux
#> 1245                  suz
#> 1246                   sv
#> 1247                  sva
#> 1248                  svm
#> 1249                   sw
#> 1250              sw-arab
#> 1251           sw-arab-cd
#> 1252           sw-arab-mz
#> 1253                sw-cd
#> 1254                  swb
#> 1255                  sxr
#> 1256                  sxu
#> 1257                  syc
#> 1258                  syl
#> 1259             syl-beng
#> 1260             syl-sylo
#> 1261                  syr
#> 1262                  szl
#> 1263                  szy
#> 1264                   ta
#> 1265                  tai
#> 1266                  tao
#> 1267                  tay
#> 1268                  tbl
#> 1269                  tce
#> 1270                  tcy
#> 1271                  tdd
#> 1272                   te
#> 1273                  tem
#> 1274                  teo
#> 1275                  ter
#> 1276                  tet
#> 1277                   tg
#> 1278              tg-cyrl
#> 1279              tg-latn
#> 1280                  tgx
#> 1281                   th
#> 1282                  thq
#> 1283                  thr
#> 1284                  tht
#> 1285                   ti
#> 1286                  tig
#> 1287                  tih
#> 1288                  tiv
#> 1289                  tji
#> 1290                   tk
#> 1291                  tkl
#> 1292                  tkr
#> 1293                   tl
#> 1294                  tlb
#> 1295                  tlh
#> 1296             tlh-latn
#> 1297             tlh-piqd
#> 1298                  tli
#> 1299                  tly
#> 1300             tly-cyrl
#> 1301                  tmh
#> 1302                  tmr
#> 1303                   tn
#> 1304                  tnq
#> 1305                   to
#> 1306                  tog
#> 1307                  toi
#> 1308                  tok
#> 1309                  tpi
#> 1310                   tr
#> 1311                  trp
#> 1312                  tru
#> 1313                  trv
#> 1314                  trw
#> 1315                   ts
#> 1316                  tsd
#> 1317                  tsg
#> 1318                  tsi
#> 1319                  tsu
#> 1320                  tsw
#> 1321                   tt
#> 1322              tt-cyrl
#> 1323              tt-latn
#> 1324                  ttj
#> 1325                  ttm
#> 1326                  ttt
#> 1327                  tui
#> 1328                  tum
#> 1329                  tup
#> 1330                  tut
#> 1331                  tvl
#> 1332                  tvu
#> 1333                   tw
#> 1334                  twd
#> 1335                  twq
#> 1336                  txa
#> 1337                  txg
#> 1338             txo-beng
#> 1339             txo-toto
#> 1340                  txx
#> 1341                   ty
#> 1342                  tyv
#> 1343                  tzl
#> 1344                  tzm
#> 1345                  tzo
#> 1346                  udm
#> 1347                   ug
#> 1348              ug-arab
#> 1349              ug-cyrl
#> 1350              ug-latn
#> 1351                  uga
#> 1352                   uk
#> 1353                  ulc
#> 1354                  uln
#> 1355                  umb
#> 1356                  umu
#> 1357                  und
#> 1358                  unr
#> 1359             unr-deva
#> 1360             unr-nagm
#> 1361                  uon
#> 1362                   ur
#> 1363                  urk
#> 1364                  ush
#> 1365                  uun
#> 1366                   uz
#> 1367              uz-cyrl
#> 1368              uz-latn
#> 1369                  uzs
#> 1370                  vai
#> 1371                   ve
#> 1372                  vec
#> 1373                  vep
#> 1374                  vgt
#> 1375                   vi
#> 1376              vi-hani
#> 1377                  vls
#> 1378               vls-be
#> 1379               vls-fr
#> 1380               vls-nl
#> 1381                  vmf
#> 1382                  vmw
#> 1383                   vo
#> 1384                  vot
#> 1385                  vro
#> 1386                  vun
#> 1387                  vut
#> 1388                   wa
#> 1389                  wae
#> 1390                  wak
#> 1391                  wal
#> 1392                  war
#> 1393                  was
#> 1394                  way
#> 1395             wbl-arab
#> 1396          wbl-arab-af
#> 1397          wbl-arab-cn
#> 1398          wbl-arab-pk
#> 1399             wbl-cyrl
#> 1400             wbl-latn
#> 1401                  wbp
#> 1402                  wen
#> 1403                  wes
#> 1404                  wlm
#> 1405                  wls
#> 1406                  wlx
#> 1407                   wo
#> 1408                  wsg
#> 1409                  wsv
#> 1410                  wuu
#> 1411             wuu-hans
#> 1412             wuu-hant
#> 1413                  wya
#> 1414                  wyi
#> 1415                  xal
#> 1416                  xbm
#> 1417                   xh
#> 1418                  xmf
#> 1419                  xmm
#> 1420                  xnb
#> 1421                  xno
#> 1422                  xnr
#> 1423             xnr-deva
#> 1424             xnr-takr
#> 1425                  xog
#> 1426                  xon
#> 1427                  xpu
#> 1428                  xsu
#> 1429                  xsy
#> 1430                  yag
#> 1431             yah-cyrl
#> 1432             yah-latn
#> 1433             yai-cyrl
#> 1434             yai-latn
#> 1435                  yao
#> 1436                  yap
#> 1437                  yas
#> 1438                  yat
#> 1439                  yav
#> 1440                  ybb
#> 1441                  ydd
#> 1442                  ydg
#> 1443                  yec
#> 1444                   yi
#> 1445                  ykg
#> 1446                   yo
#> 1447                  yoi
#> 1448             yoi-hira
#> 1449                  yox
#> 1450             yox-hira
#> 1451                  ypk
#> 1452                  yrk
#> 1453                  yrl
#> 1454                  yua
#> 1455                  yue
#> 1456             yue-hans
#> 1457             yue-hant
#> 1458                   za
#> 1459                  zai
#> 1460                  zap
#> 1461                  zbl
#> 1462                  zea
#> 1463                  zen
#> 1464                  zgh
#> 1465             zgh-latn
#> 1466                   zh
#> 1467         zh-classical
#> 1468                zh-cn
#> 1469              zh-hans
#> 1470              zh-hant
#> 1471                zh-hk
#> 1472           zh-min-nan
#> 1473                zh-mo
#> 1474                zh-my
#> 1475                zh-sg
#> 1476                zh-tw
#> 1477               zh-yue
#> 1478                  zmi
#> 1479                  znd
#> 1480                  zpu
#> 1481                   zu
#> 1482                  zun
#> 1483                  zxx
#> 1484                  zza
#>                                                          name
#> 1                                                        Afar
#> 2                                                    Arbëresh
#> 3                                                   Abkhazian
#> 4                                             Western Abenaki
#> 5                                                       Abaza
#> 6                                                       Abaza
#> 7                                                       Abron
#> 8                                              Ambonese Malay
#> 9                                                       Abure
#> 10                                                   Acehnese
#> 11                                        Saint Lucian Creole
#> 12                                                      Acoli
#> 13                                               Iraqi Arabic
#> 14                                                    Adangme
#> 15                                              Andegerebinha
#> 16                                                    Adjukru
#> 17                                                     Adyghe
#> 18                                   Adyghe (Cyrillic script)
#> 19                                      Adyghe (Latin script)
#> 20                                                    Avestan
#> 21                                            Tunisian Arabic
#> 22                            Tunisian Arabic (Arabic script)
#> 23                             Tunisian Arabic (Latin script)
#> 24                                              Saʽidi Arabic
#> 25                                           Northeast Pashai
#> 26                                           Eastern Arrernte
#> 27                                                  Afrikaans
#> 28                                      Afroasiatic languages
#> 29                                                   Afrihili
#> 30                                                      Aghem
#> 31                                                     Ahanta
#> 32                                                    Ahirani
#> 33                       Antiguan and Barbudan Creole English
#> 34                                       Assyrian Neo-Aramaic
#> 35                                                       Ainu
#> 36                                                     Ajagbe
#> 37                                     South Levantine Arabic
#> 38                     South Levantine Arabic (Arabic script)
#> 39                      South Levantine Arabic (Latin script)
#> 40                                                       Akan
#> 41                                              Batak Angkola
#> 42                                                   Akkadian
#> 43                                    Akkadian (Latin script)
#> 44                                Akkadian (Cuneiform script)
#> 45                                                    Alabama
#> 46                                                   Kawésqar
#> 47                                                      Aleut
#> 48                                    Aleut (Cyrillic script)
#> 49                                       Algonquian languages
#> 50                                              Gheg Albanian
#> 51                                                  Algonquin
#> 52                                                  Alemannic
#> 53                                             Southern Altai
#> 54                                                   Alyawarr
#> 55                                                    Amharic
#> 56                                                       Amis
#> 57                                                 Anmatyerre
#> 58                                                  Aragonese
#> 59                                                    Xârâcùù
#> 60                                                Old English
#> 61                                                      Obolo
#> 62                                                     Angika
#> 63                                        Southern Athabaskan
#> 64                                           Levantine Arabic
#> 65                           Levantine Arabic (Arabic script)
#> 66                            Levantine Arabic (Latin script)
#> 67                                             Western Apache
#> 68                                                     Arabic
#> 69                                     Modern Standard Arabic
#> 70                                                    Aramaic
#> 71                                           Western Arrarnta
#> 72                                                    Mapuche
#> 73                                                     Araona
#> 74                                                    Arapaho
#> 75                                            Algerian Arabic
#> 76                                               Najdi Arabic
#> 77                                      constructed languages
#> 78                                                     Arawak
#> 79                                            Moroccan Arabic
#> 80                            Moroccan Arabic (Arabic script)
#> 81                             Moroccan Arabic (Latin script)
#> 82                                            Egyptian Arabic
#> 83                                                   Assamese
#> 84                                                        Asu
#> 85                                     American Sign Language
#> 86                                                   Asturian
#> 87                                       Athabaskan languages
#> 88                                                      Attié
#> 89                                                  Atikamekw
#> 90                                             Northern Altai
#> 91                            Australian Aboriginal languages
#> 92                                                     Avaric
#> 93                                                     Avikam
#> 94                                                     Kotava
#> 95                                                     Awadhi
#> 96                                                Ayerrerenge
#> 97                                      Lower Southern Aranda
#> 98                                                     Aymara
#> 99                                            Hadhrami Arabic
#> 100                                                   Maybrat
#> 101                                               Azerbaijani
#> 102                               Azerbaijani (Arabic script)
#> 103                             Azerbaijani (Cyrillic script)
#> 104                                Azerbaijani (Latin script)
#> 105                                         South Azerbaijani
#> 106                                         North Azerbaijani
#> 107                                                   Bashkir
#> 108                                           Banda languages
#> 109                                                      Tuki
#> 110                                        Bamileke languages
#> 111                                                   Baluchi
#> 112                                    Baluchi (Latin script)
#> 113                                                  Balinese
#> 114                                Balinese (Balinese script)
#> 115                                                  Bavarian
#> 116                                                     Basaa
#> 117                                          Baltic languages
#> 118                                                Samogitian
#> 119                                                     Bamun
#> 120                                                Batak Toba
#> 121                                 Batak Toba (Batak script)
#> 122                                 Batak Toba (Latin script)
#> 123                                                   Ghomala
#> 124                                          Southern Balochi
#> 125                                                    Baoulé
#> 126                                             Central Bikol
#> 127                                          West Coast Bajau
#> 128                                                Belarusian
#> 129                     Belarusian (Taraškievica orthography)
#> 130                     Belarusian (Taraškievica orthography)
#> 131                                                      Beja
#> 132                                                     Bemba
#> 133                                          Berber languages
#> 134                                                    Betawi
#> 135                                                      Bena
#> 136                                                      Bari
#> 137                                                     Bafut
#> 138                                     British Sign Language
#> 139                                                    Badaga
#> 140                                                     Balti
#> 141                                    Balti (Tibetan script)
#> 142                                                     Bonda
#> 143                                             Mahasu Pahari
#> 144                         Mahasu Pahari (Devanagari script)
#> 145                              Mahasu Pahari (Takri script)
#> 146                                                 Bulgarian
#> 147                                                  Haryanvi
#> 148                                  Haryanvi (Arabic script)
#> 149                              Haryanvi (Devanagari script)
#> 150                                           Western Balochi
#> 151                                           Eastern Balochi
#> 152                                                     Bagri
#> 153                                     Bagri (Arabic script)
#> 154                                 Bagri (Devanagari script)
#> 155                                                  Bhojpuri
#> 156                                                    Bharia
#> 157                                                Bhadrawahi
#> 158                            Bhadrawahi (Devanagari script)
#> 159                                 Bhadrawahi (Takri script)
#> 160                                                  Bhojpuri
#> 161                                                   Bislama
#> 162                                                     Bikol
#> 163                                                      Bini
#> 164                                                    Banjar
#> 165                                                      Baka
#> 166                                                    Bakoko
#> 167                                                       Kom
#> 168                                                   Bukitan
#> 169                                                   Siksiká
#> 170                                                    Nuxalk
#> 171                                                      Pa'O
#> 172                                                      Anii
#> 173                                                   Tai Dam
#> 174                                                   Bambara
#> 175                                                    Bangla
#> 176                             Bangla (Sylheti Nagri script)
#> 177                                                    Bookan
#> 178                                                     Bunun
#> 179                                           Bantu languages
#> 180                                                   Bintulu
#> 181                                                   Tibetan
#> 182                                                      Bole
#> 183                                                     Berom
#> 184                                               Bishnupriya
#> 185                                                 Bakhtiari
#> 186                                                     Mka'a
#> 187                                                    Breton
#> 188                                                      Braj
#> 189                                                    Brahui
#> 190                                     Brahui (Latin script)
#> 191                                                      Bodo
#> 192                                                   Bosnian
#> 193                                                     Wushi
#> 194                                                Burushaski
#> 195                                                    Akoose
#> 196                                               Batak Dairi
#> 197                                                    Biatah
#> 198                                           Batak languages
#> 199                                          Batak Mandailing
#> 200                                           Rinconada Bikol
#> 201                                          Batak Simalungun
#> 202                                                Batak Karo
#> 203                                          Batak Alas-Kluet
#> 204                                                    Buriat
#> 205                                                  Buginese
#> 206                                Buginese (Buginese script)
#> 207                                                      Bulu
#> 208                                                      Bube
#> 209                                                Bura-Pabir
#> 210                                             Russia Buriat
#> 211                                                      Blin
#> 212                                                   Medumba
#> 213                                              Belize Kriol
#> 214                                   Brazilian Sign Language
#> 215                                                   Catalan
#> 216                                                     Caddo
#> 217                                    Mesoamerican languages
#> 218                                                 Kaqchikel
#> 219                                                Carolinian
#> 220                                                     Carib
#> 221                                       Caucasian languages
#> 222                                                    Cayuga
#> 223                                                 Chavacano
#> 224                                                 Chavacano
#> 225                                                     Atsam
#> 226                                                    Chakma
#> 227                                   Chakma (Bengali script)
#> 228                                                   Mindong
#> 229                                      Mindong (Han script)
#> 230                          Mindong (Traditional Han script)
#> 231                                    Mindong (Latin script)
#> 232                                     Koda (Bengali script)
#> 233                                                   Chechen
#> 234                                                   Cebuano
#> 235                                          Celtic languages
#> 236                                                     Chiga
#> 237                                                  Chamorro
#> 238                                                   Chibcha
#> 239                                                  Chagatai
#> 240                                                  Chuukese
#> 241                                                      Mari
#> 242                                            Chinook Jargon
#> 243                                                   Choctaw
#> 244                                                 Chipewyan
#> 245                                                  Cherokee
#> 246                                                  Cheyenne
#> 247                                                 Chickasaw
#> 248                                                  Chippewa
#> 249                                              Western Cham
#> 250                              Western Cham (Arabic script)
#> 251                                Western Cham (Cham script)
#> 252                               Western Cham (Latin script)
#> 253                                              Eastern Cham
#> 254                              Eastern Cham (Arabic script)
#> 255                                Eastern Cham (Cham script)
#> 256                               Eastern Cham (Latin script)
#> 257                                                       Jin
#> 258                               Jin (Simplified Han script)
#> 259                              Jin (Traditional Han script)
#> 260                                           Central Kurdish
#> 261                           Central Kurdish (Arabic script)
#> 262                            Central Kurdish (Latin script)
#> 263                                                     Anufo
#> 264                                                   Chukchi
#> 265                                                   Kavalan
#> 266                                                 Chilcotin
#> 267                                          Chamic languages
#> 268                                       Classical Mongolian
#> 269              Mandarin (Latin script, China, Hanyu Pinyin)
#> 270             Mandarin (Latin script, Taiwan, Hanyu Pinyin)
#> 271          Mandarin (Latin script, Taiwan, Tongyong Pinyin)
#> 272  Mandarin (Latin script, Taiwan, Wade-Giles romanization)
#> 273                                                Hakha-Chin
#> 274                                               Montenegrin
#> 275                             Montenegrin (Cyrillic script)
#> 276                                Montenegrin (Latin script)
#> 277                                            Middle Cornish
#> 278                                                  Corsican
#> 279                                               Cocos Malay
#> 280                                                    Coptic
#> 281                            English-based creole languages
#> 282                             French-based creole languages
#> 283                         Portuguese-based creole languages
#> 284                                                  Capiznon
#> 285                                                    Puxian
#> 286                            Puxian (Simplified Han script)
#> 287                           Puxian (Traditional Han script)
#> 288                                     Puxian (Latin script)
#> 289                                                      Cree
#> 290                      Cree (Canadian Aboriginal syllabics)
#> 291                                       Cree (Latin script)
#> 292                                              Island Carib
#> 293                                                    Michif
#> 294                                             Crimean Tatar
#> 295                           Crimean Tatar (Cyrillic script)
#> 296                              Crimean Tatar (Latin script)
#> 297                                            Dobrujan Tatar
#> 298                                        Southern East Cree
#> 299                                               Plains Cree
#> 300                                        Northern East Cree
#> 301                                                Moose Cree
#> 302                                       creoles and pidgins
#> 303                                       Carolina Algonquian
#> 304                                     Seselwa Creole French
#> 305                                                     Czech
#> 306                                                 Kashubian
#> 307                                               Swampy Cree
#> 308                                              Chittagonian
#> 309                                             Church Slavic
#> 310                                        Cushitic languages
#> 311                                                   Chuvash
#> 312                                                     Welsh
#> 313                                                    Danish
#> 314                                                   Dagbani
#> 315                                                    Dakota
#> 316                                                    Dargwa
#> 317                                                     Taita
#> 318                                      Land Dayak languages
#> 319                                                    Idaʼan
#> 320                                                     Dendi
#> 321                                                    German
#> 322                          German (traditional orthography)
#> 323                                           Austrian German
#> 324                                         Swiss High German
#> 325                                   German (formal address)
#> 326                                                  Delaware
#> 327                                                     Slave
#> 328                                          Southern Dagaare
#> 329                                                    Dogrib
#> 330                                                     Dinka
#> 331                                                     Dimli
#> 332                                                     Zarma
#> 333                                                    Ndyuka
#> 334                                                    Kuijau
#> 335                                                    Dolgan
#> 336                                        Upper Kinabatangan
#> 337                                                    Dumpas
#> 338                                                     Dogri
#> 339                                     Dogri (Arabic script)
#> 340                                 Dogri (Devanagari script)
#> 341                                      Dogri (Dogra script)
#> 342                                                     Papar
#> 343                                       Dravidian languages
#> 344                                                    Rungus
#> 345                                                 Daro-Matu
#> 346                                                     Rukai
#> 347                                             Lower Sorbian
#> 348                                                    Desiya
#> 349                                           Eastern Kadazan
#> 350                                             Central Dusun
#> 351                                                     Lotud
#> 352                                                    Doteli
#> 353                                                     Duala
#> 354                                                    Dumbea
#> 355                                              Middle Dutch
#> 356                                                    Divehi
#> 357                                                Jola-Fonyi
#> 358                                                     Dyula
#> 359                                                  Dzongkha
#> 360                                                    Dazaga
#> 361                                                      Embu
#> 362                                                       Ewe
#> 363                                                      Efik
#> 364                                        Emiliano-Romagnolo
#> 365                                          Ancient Egyptian
#> 366                                                    Ekajuk
#> 367                                                    Ekpeye
#> 368                                                     Greek
#> 369                                             Cypriot Greek
#> 370                                                     Eleme
#> 371                                                   Elamite
#> 372                                                 Emerillon
#> 373                                        Emiliano-Romagnolo
#> 374                                                   English
#> 375                                        Australian English
#> 376                                          Canadian English
#> 377                                  English (Deseret script)
#> 378                                      Early Modern English
#> 379                                           British English
#> 380                                            Indian English
#> 381                                          Jamaican English
#> 382                                       New Zealand English
#> 383                                  English (Shavian script)
#> 384                                            Simple English
#> 385                                           British English
#> 386                                          American English
#> 387                                            Middle English
#> 388                                                 Esperanto
#> 389                          Esperanto (h-system orthography)
#> 390                                Esperanto (Shavian script)
#> 391                          Esperanto (x-system orthography)
#> 392                                                   Spanish
#> 393                                    Latin American Spanish
#> 394                                          European Spanish
#> 395                                  Spanish (formal address)
#> 396                                           Mexican Spanish
#> 397                                       Spanish (Nicaragua)
#> 398                                    Central Siberian Yupik
#> 399                                             Central Yupik
#> 400                                                  Estonian
#> 401                                                      Eton
#> 402                                                  Etruscan
#> 403                                                   Ejagham
#> 404                                                    Basque
#> 405                                                    Ewondo
#> 406                                              Extremaduran
#> 407                                                      Eyak
#> 408                                                   Persian
#> 409                                      Persian (South Asia)
#> 410                                                      Dari
#> 411                                         Annobonese Creole
#> 412                                                      Fang
#> 413                                                     Fanti
#> 414                                                      Fala
#> 415                                                 Kuhmareyi
#> 416                                                      Fula
#> 417                                                   Finnish
#> 418                                                  Filipino
#> 419                                        Tornedalen Finnish
#> 420                                     Finno-Ugric languages
#> 421                                                      Võro
#> 422                                                    Fijian
#> 423                                                    Kvensk
#> 424                                                    Fe'Fe'
#> 425                                                   Faroese
#> 426                                                       Fon
#> 427                                                    Siraya
#> 428                                                    French
#> 429                                            Belgian French
#> 430                                           Canadian French
#> 431                                              Swiss French
#> 432                                              Cajun French
#> 433                                                  Frankish
#> 434                                             Middle French
#> 435                                                Old French
#> 436                                                   Arpitan
#> 437                                          Northern Frisian
#> 438                                 Eastern Frisian Low Saxon
#> 439                                      French Sign Language
#> 440                                                   Futunan
#> 441                                                     Pular
#> 442                                                  Friulian
#> 443                                                       Fur
#> 444                                           Western Frisian
#> 445                                                     Irish
#> 446                                                        Ga
#> 447                                                    Gagauz
#> 448                                                   Alekano
#> 449                                                       Gan
#> 450                               Gan (Simplified Han script)
#> 451                              Gan (Traditional Han script)
#> 452                                                      Gayo
#> 453                                                     Gbaya
#> 454                                                  Kaytetye
#> 455                                                     Gaddi
#> 456                                 Gaddi (Devanagari script)
#> 457                                      Gaddi (Takri script)
#> 458                                                  Garhwali
#> 459                                          Zoroastrian Dari
#> 460                                       Guadeloupean Creole
#> 461                                            Guianan Creole
#> 462                                           Scottish Gaelic
#> 463                                        Germanic languages
#> 464                                                      Geez
#> 465                                                Gilbertese
#> 466                                                    Gujari
#> 467                                    Gujari (Arabic script)
#> 468                                Gujari (Devanagari script)
#> 469                                                  Galician
#> 470                                                     Nanai
#> 471                                          Northwest Pashai
#> 472                                                    Gilaki
#> 473                                        Middle High German
#> 474                                         Middle Low German
#> 475                                           Mycenaean Greek
#> 476                                                   Guarani
#> 477                                                     Ganaʼ
#> 478                                           Old High German
#> 479                                              Goan Konkani
#> 480                          Goan Konkani (Devanagari script)
#> 481                               Goan Konkani (Latin script)
#> 482                                                     Gondi
#> 483                                                 Gorontalo
#> 484                                                    Gothic
#> 485                                           Ghanaian Pidgin
#> 486                                                     Grebo
#> 487                                             Ancient Greek
#> 488                                      German Sign Language
#> 489                                                 Alemannic
#> 490                                                  Alsatian
#> 491                                                  Gujarati
#> 492                                                     Wayuu
#> 493                                                 Guambiano
#> 494                                                    Frafra
#> 495                                                       Gun
#> 496                                                     Gusii
#> 497                                                      Manx
#> 498                                                  Gwichʼin
#> 499                                                     Gbaya
#> 500                                                     Hausa
#> 501                                     Hausa (Arabic script)
#> 502                                      Hausa (Latin script)
#> 503                                             Hausa (Niger)
#> 504                                                    Gurani
#> 505                                                     Haida
#> 506                                             Hakka Chinese
#> 507                             Hakka (Simplified Han script)
#> 508                            Hakka (Traditional Han script)
#> 509                                      Hakka (Latin script)
#> 510                                                      Havu
#> 511                                                  Hawaiian
#> 512                                            Southern Haida
#> 513                                                  Hazaragi
#> 514                                           Biblical Hebrew
#> 515                                                    Hebrew
#> 516                                    Northern Qiandong Miao
#> 517                                                     Hindi
#> 518                                     Hindi (Kaithi script)
#> 519                                             Hindi (Latin)
#> 520                                                Fiji Hindi
#> 521                            Fiji Hindi (Devanagari script)
#> 522                                 Fiji Hindi (Latin script)
#> 523                                                Hiligaynon
#> 524                                            Western Pahari
#> 525                                                   Hittite
#> 526                                    Hittite (Latin script)
#> 527                                Hittite (Cuneiform script)
#> 528                                                     Hunde
#> 529                                                     Hmong
#> 530                                             Chhattisgarhi
#> 531                                                Hmong Njua
#> 532                                           Northern Hindko
#> 533                                                 Hiri Motu
#> 534                                                        Ho
#> 535                                         Ho (Latin script)
#> 536                                                  Croatian
#> 537                              Croatian (Glagolitic script)
#> 538                                                   Hunsrik
#> 539                                             Upper Sorbian
#> 540                                                     Xiang
#> 541                             Xiang (Simplified Han script)
#> 542                            Xiang (Traditional Han script)
#> 543                                            Haitian Creole
#> 544                                                     Hadza
#> 545                                                 Hungarian
#> 546                                Hungarian (formal address)
#> 547                                                      Hupa
#> 548                                                Halkomelem
#> 549                                                  Armenian
#> 550                                          Western Armenian
#> 551                                                    Herero
#> 552                                               Interlingua
#> 553                                                      Iban
#> 554                                                    Ibibio
#> 555                                                Indonesian
#> 556                                               Interlingue
#> 557                                            Mayoyao Ifugao
#> 558                                                      Igbo
#> 559                                                     Ebira
#> 560                                                     Igala
#> 561                                                Sichuan Yi
#> 562                                            Ijaw languages
#> 563                                                   Inupiaq
#> 564                   Eastern Canadian (Aboriginal syllabics)
#> 565                           Eastern Canadian (Latin script)
#> 566                                Western Canadian Inuktitut
#> 567                                                     Iloko
#> 568                                      Indo-Aryan languages
#> 569                                   Indo-European languages
#> 570                                                    Ingush
#> 571                                                       Ido
#> 572                                         Iranian languages
#> 573                                       Iroquoian languages
#> 574                                                 Icelandic
#> 575                                                      Esan
#> 576                                Ishkashimi (Arabic script)
#> 577                              Ishkashimi (Cyrillic script)
#> 578                                 Ishkashimi (Latin script)
#> 579                                                   Istriot
#> 580                                                       Isu
#> 581                                               Interslavic
#> 582                             Interslavic (Cyrillic script)
#> 583                                Interslavic (Latin script)
#> 584                                                   Italian
#> 585                                                 Inuktitut
#> 586                                                    Ibatan
#> 587                                                   Ingrian
#> 588                                                     Izere
#> 589                                                  Japanese
#> 590                                   Japanese (Kanji script)
#> 591                                Japanese (Hiragana script)
#> 592                                    Japanese (Kana script)
#> 593                                Japanese (Katakana script)
#> 594                                                    Popti'
#> 595                                                     Jakun
#> 596                                   Jamaican Creole English
#> 597                                               Jambi Malay
#> 598                                                    Lojban
#> 599                                                 Judeo-Tat
#> 600                               Judeo-Tat (Cyrillic script)
#> 601                                                    Ngomba
#> 602                                                      Jeju
#> 603                                                   Machame
#> 604                                             Judeo-Persian
#> 605                                              Judeo-Arabic
#> 606                                                     Wapan
#> 607                                                    Jutish
#> 608                                                  Javanese
#> 609                                Javanese (Javanese script)
#> 610                                                  Georgian
#> 611                                               Kara-Kalpak
#> 612                                                    Kabyle
#> 613                                                    Kachin
#> 614                                                   Kajaman
#> 615                                                  Karekare
#> 616                                                       Jju
#> 617                                                     Kamba
#> 618                                         Karenic languages
#> 619                                                      Kawi
#> 620                                                 Kabardian
#> 621                               Kabardian (Cyrillic script)
#> 622                                  Kabardian (Latin script)
#> 623                                                   Kanembu
#> 624                                                    Kabiye
#> 625                                                      Tyap
#> 626                                                   Kalanga
#> 627                                                   Makonde
#> 628                                       Cape Verdean Creole
#> 629                                                  Qʼeqchiʼ
#> 630                                                   Kenyang
#> 631                                                      Kera
#> 632                                                      Koro
#> 633                                                    Kutchi
#> 634                                                     Kongo
#> 635                                                  Komering
#> 636                                  Komering (Arabic script)
#> 637                                                   Kusunda
#> 638                                                  Kaingang
#> 639                                                     Khasi
#> 640                                         Khoisan languages
#> 641                                                 Khotanese
#> 642                                              Koyra Chiini
#> 643                                                    Khowar
#> 644                                                    Kikuyu
#> 645                                               Sheshi Kham
#> 646                                                 Kirmanjki
#> 647                                         Khiamniungan Naga
#> 648                                                  Kuanyama
#> 649                                                    Khakas
#> 650                                               Eastern Pwo
#> 651                                                    Kazakh
#> 652                                    Kazakh (Arabic script)
#> 653                                            Kazakh (China)
#> 654                                  Kazakh (Cyrillic script)
#> 655                                       Kazakh (Kazakhstan)
#> 656                                     Kazakh (Latin script)
#> 657                                           Kazakh (Turkey)
#> 658                                                      Kako
#> 659                                               Kalaallisut
#> 660                                                Gamilaraay
#> 661                                                  Kalenjin
#> 662                                                   Kalasha
#> 663                                   Kalasha (Arabic script)
#> 664                                    Kalasha (Latin script)
#> 665                                                     Khmer
#> 666                                                  Kimbundu
#> 667                                          Northern Kurdish
#> 668                          Northern Kurdish (Arabic script)
#> 669                           Northern Kurdish (Latin script)
#> 670                                          Khorasani Turkic
#> 671                                                   Kannada
#> 672                                            Central Kanuri
#> 673                                                 Kankanaey
#> 674                                     Maharashtrian Konkani
#> 675                                                    Kintaq
#> 676                                                    Korean
#> 677                                            Korean (China)
#> 678                                     Korean (Hanja script)
#> 679                                     Korean (mixed script)
#> 680                                      Korean (North Korea)
#> 681                                      Korean (South Korea)
#> 682                                              Komi-Permyak
#> 683                                                   Konkani
#> 684                                                  Kosraean
#> 685                                                   Koyukon
#> 686                                                    Kpelle
#> 687                                                Kimaragang
#> 688                                       Klias River Kadazan
#> 689                                                    Okolod
#> 690                                                    Kanuri
#> 691                                           Karachay-Balkar
#> 692                                                      Krio
#> 693                                                 Kinaray-a
#> 694                                                  Karelian
#> 695                                             Kru languages
#> 696                                                    Kurukh
#> 697                                                  Kashmiri
#> 698                                  Kashmiri (Arabic script)
#> 699                              Kashmiri (Devanagari script)
#> 700                                                  Shambala
#> 701                                                     Bafia
#> 702                                                 Colognian
#> 703                                               S'gaw Karen
#> 704                              Kharia Thar (Bengali script)
#> 705                                                   Kurdish
#> 706                                   Kurdish (Arabic script)
#> 707                                    Kurdish (Latin script)
#> 708                                                     Kumyk
#> 709                                                    Kusaal
#> 710                                                   Kutenai
#> 711                                                      Komi
#> 712                                                 Kalabakan
#> 713                                                   Cornish
#> 714                                                 Kwakʼwala
#> 715                                              Brunei Malay
#> 716                                            Keningau Murut
#> 717                                                   Kanowit
#> 718                                                      Kuvi
#> 719                                                    Kyrgyz
#> 720                                  Kurmali (Bengali script)
#> 721                               Kurmali (Devanagari script)
#> 722                                                     Latin
#> 723                                                    Ladino
#> 724                                    Ladino (Hebrew script)
#> 725                                     Ladino (Latin script)
#> 726                                                     Langi
#> 727                                           Western Panjabi
#> 728                                                     Lango
#> 729                                                     Lamba
#> 730                                             Luxembourgish
#> 731                                                       Lak
#> 732                                                    Tungag
#> 733                                                    Láadan
#> 734                                                  Nomaande
#> 735                                                  Lezghian
#> 736                                        Lingua Franca Nova
#> 737                                                     Ganda
#> 738                                                Limburgish
#> 739                                        Belgian Limburgish
#> 740                                          Dutch Limburgish
#> 741                                                  Ligurian
#> 742                                                Monégasque
#> 743                                                  Lillooet
#> 744                                                  Livonian
#> 745                                               Lampung Api
#> 746                                                      Laki
#> 747                                                    Lakota
#> 748                                                     Ladin
#> 749                                                   Lambadi
#> 750                               Lambadi (Devanagari script)
#> 751                                  Lambadi (Kannada script)
#> 752                                    Lambadi (Tamil script)
#> 753                                   Lambadi (Telugu script)
#> 754                                                   Lombard
#> 755                                                   Lingala
#> 756                                                   Lamnso'
#> 757                                                       Lao
#> 758                                                     Mongo
#> 759                                                      Loma
#> 760                                          Louisiana Creole
#> 761                                                      Lozi
#> 762                                             Northern Luri
#> 763                                                    Saamia
#> 764                                                Lithuanian
#> 765                                                 Latgalian
#> 766                                              Luba-Katanga
#> 767                                                Luba-Lulua
#> 768                                                     Ludic
#> 769                                                   Luiseno
#> 770                                                     Lunda
#> 771                                                       Luo
#> 772                                                      Mizo
#> 773                                               Lushootseed
#> 774                                                     Luyia
#> 775                                             Southern Luri
#> 776                                                   Latvian
#> 777                                          Literary Chinese
#> 778                                                       Laz
#> 779                                                  Madurese
#> 780                                                      Mafa
#> 781                                                    Magahi
#> 782                                                  Maithili
#> 783                                                   Makasar
#> 784                                 Makasar (Buginese script)
#> 785                                                  Mandingo
#> 786                                    Austronesian languages
#> 787                                                Banyumasan
#> 788                                                     Masai
#> 789                                                  Mampruli
#> 790                                                     Massa
#> 791                                                      Maka
#> 792                                                      Maba
#> 793                                                    Moksha
#> 794                                              Maguindanaon
#> 795                                                    Mandar
#> 796                                                     Mende
#> 797                                                      Meru
#> 798                                                Hassaniyya
#> 799                                    Kelantan-Pattani Malay
#> 800                                                  Morisyen
#> 801                                                  Malagasy
#> 802                                              Middle Irish
#> 803                                            Makhuwa-Meetto
#> 804                                                     Metaʼ
#> 805                                               Marshallese
#> 806                                                   Mungaka
#> 807                                                   Mòcheno
#> 808                                              Eastern Mari
#> 809                                                     Māori
#> 810                                                   Mi'kmaw
#> 811                                                   Mandaic
#> 812                                               Minangkabau
#> 813                                                   Miskito
#> 814                                      unsupported language
#> 815                                                    Mixtec
#> 816                                        Northwestern Maidu
#> 817                                   Mahali (Bengali script)
#> 818                                                Macedonian
#> 819                                                 Mon-Khmer
#> 820                                                 Malayalam
#> 821                                                 Mongolian
#> 822                               Mongolian (Cyrillic script)
#> 823                              Mongolian (Mongolian script)
#> 824                                                    Manchu
#> 825                                     Manchu (Latin script)
#> 826                                 Manchu (Mongolian script)
#> 827                                                  Manipuri
#> 828                                 Manipuri (Bengali script)
#> 829                                                     Munji
#> 830                                          Manobo languages
#> 831                                                    Minriq
#> 832                                                     Mansi
#> 833                                                       Mon
#> 834                                                  Moldovan
#> 835                                                Innu-aimun
#> 836                                                    Mohawk
#> 837                                                     Mossi
#> 838                                                   Marathi
#> 839                                     Marathi (Modi script)
#> 840                                                      Mara
#> 841                                              Western Mari
#> 842                                            Marghi Central
#> 843                                                 Mangareva
#> 844                                                     Malay
#> 845                                       Malay (Jawi script)
#> 846                                               Sabah Malay
#> 847                                                   Maltese
#> 848                                                   Mundang
#> 849                                                      Musi
#> 850                                        multiple languages
#> 851                                           Munda languages
#> 852                                                  Muscogee
#> 853                                      Peripheral Mongolian
#> 854                                                    Miyako
#> 855                                  Miyako (Hiragana script)
#> 856                                                     Tagol
#> 857                                                 Mirandese
#> 858                                                   Marwari
#> 859                                                  Mentawai
#> 860                                                 Hmong Daw
#> 861                                  Hmong Daw (Latin script)
#> 862                                                   Burmese
#> 863                                                     Myene
#> 864                                           Mayan languages
#> 865                                                     Erzya
#> 866                                               Mazanderani
#> 867                                                     Nauru
#> 868                                                   Nahuatl
#> 869                     Indigenous languages of North America
#> 870                                                    Minnan
#> 871                                       Minnan (Han script)
#> 872                            Minnan (Simplified Han script)
#> 873                           Minnan (Traditional Han script)
#> 874                                        Minnan (Pe̍h-ōe-jī)
#> 875                                           Minnan (Tâi-lô)
#> 876                                                Neapolitan
#> 877                                                      Nama
#> 878                                          Norwegian Bokmål
#> 879                                             North Ndebele
#> 880                                                Low German
#> 881                                                 Low Saxon
#> 882                                                    Nepali
#> 883                                                    Newari
#> 884                                                    Ndonga
#> 885                                                    Ngémba
#> 886                                                      Nias
#> 887                                     Niger–Congo languages
#> 888                                       Southeastern Kolami
#> 889                                                    Niuean
#> 890                                                   Ao Naga
#> 891                                                     Dutch
#> 892                                              Aruban Dutch
#> 893                                             Belgian Dutch
#> 894                                           Curaçaoan Dutch
#> 895                                  Dutch (informal address)
#> 896                                         Netherlands Dutch
#> 897                                          Surinamese Dutch
#> 898                                        Sint Maarten Dutch
#> 899                                            Brussels Dutch
#> 900                                                  Ngombala
#> 901                                                    Kwasio
#> 902                                                     Nawdm
#> 903                                         Norwegian Nynorsk
#> 904                                        Norwegian Høgnorsk
#> 905                                                 Ngiemboon
#> 906                                                  Nda'Nda'
#> 907                                                 Norwegian
#> 908                                             Northern Thai
#> 909                               Northern Thai (Thai script)
#> 910                                                     Nogai
#> 911                                                 Old Norse
#> 912                                  Old Norse (Runic script)
#> 913                                                    Novial
#> 914                                                      N’Ko
#> 915                                             South Ndebele
#> 916                                               Guernésiais
#> 917                                                  Jèrriais
#> 918                                                    Norman
#> 919                                                   Naskapi
#> 920                                   Norwegian Sign Language
#> 921                                            Northern Sotho
#> 922                                            Sesayap Tidung
#> 923                                          Nubian languages
#> 924                                                      Nupe
#> 925                                                      Nuer
#> 926                                                    Navajo
#> 927                                          Classical Newari
#> 928                                                  Numidian
#> 929                                                    Nyanja
#> 930                                                  Nyamwezi
#> 931                                                  Nyankole
#> 932                                                     Nyoro
#> 933                                                   Nyungar
#> 934                                                     Nzima
#> 935                                                Old Breton
#> 936                                                   Occitan
#> 937                                               Old Cornish
#> 938                                                 Old Dutch
#> 939                                               Old Frisian
#> 940                                                    Ojibwa
#> 941                                       Northwestern Ojibwa
#> 942                                            Central Ojibwa
#> 943                                              Old Japanese
#> 944                               Old Japanese (Kanji script)
#> 945                            Old Japanese (Hiragana script)
#> 946                                                  Oji-Cree
#> 947                                            Western Ojibwa
#> 948                                                  Okanagan
#> 949                                            Livvi-Karelian
#> 950                                                     Oromo
#> 951                                               Omaha-Ponca
#> 952                                                Old Nubian
#> 953                                                   O'odham
#> 954                                                      Odia
#> 955                                                   Ossetic
#> 956                                                     Osage
#> 957                                      Osage (Latin script)
#> 958                                                     Osing
#> 959                                                 Old Saxon
#> 960                                           Ottoman Turkish
#> 961                                               Old Turkish
#> 962                                         Otomian languages
#> 963                                                 Elfdalian
#> 964                                                 Old Welsh
#> 965                                                   Wayampi
#> 966                                                   Punjabi
#> 967                                 Punjabi (Gurmukhi script)
#> 968                                          Papuan languages
#> 969                                                Pangasinan
#> 970                                                   Pahlavi
#> 971                    Pahlavi (Inscriptional Pahlavi script)
#> 972                          Pahlavi (Psalter Pahlavi script)
#> 973                             Pahlavi (Book Pahlavi script)
#> 974                                                  Pampanga
#> 975                                           Northern Paiute
#> 976                                                Papiamento
#> 977                                        Papiamento (Aruba)
#> 978                                                     Parya
#> 979                                                   Palauan
#> 980                                                      Páez
#> 981                                                    Picard
#> 982                                            Belgian Picard
#> 983                                             French Picard
#> 984                                           Nigerian Pidgin
#> 985                                       Pennsylvania German
#> 986                                              Plautdietsch
#> 987                                               Old Persian
#> 988                                           Palatine German
#> 989                                                  Gāndhārī
#> 990                                  Gāndhārī (Arabic script)
#> 991                              Gāndhārī (Devanagari script)
#> 992                              Gāndhārī (Kharoshthi script)
#> 993                                           Primitive Irish
#> 994                                      Philippine languages
#> 995                                                    Palula
#> 996                                                Phoenician
#> 997                                 Phoenician (Latin script)
#> 998                            Phoenician (Phoenician script)
#> 999                                            Pahari-Potwari
#> 1000                                                     Pali
#> 1001                                    Pali (Siddham script)
#> 1002                                         Pitcairn-Norfolk
#> 1003                                                    Pijin
#> 1004                                           Pitjantjatjara
#> 1005                                                  Paekche
#> 1006                                                   Pökoot
#> 1007                                   Pakistan Sign Language
#> 1008                                                   Polish
#> 1009                                                  Palikur
#> 1010                                       Southwest Palawano
#> 1011                                  Brooke's Point Palawano
#> 1012                                              Piedmontese
#> 1013                                          Western Punjabi
#> 1014                                                   Pontic
#> 1015                                                Pohnpeian
#> 1016                                     Upper Guinea Crioulo
#> 1017                                                    Nawat
#> 1018                                            Papora-Hoanya
#> 1019                                   Maliseet-Passamaquoddy
#> 1020                                                  Prakrit
#> 1021                                                  Parachi
#> 1022                                                 Prussian
#> 1023                                            Old Provençal
#> 1024                                                     Dari
#> 1025                                                   Pashto
#> 1026                                     Pashto (Afghanistan)
#> 1027                                        Pashto (Pakistan)
#> 1028                                         Southwest Pashai
#> 1029                                         Southeast Pashai
#> 1030                                        Sauraseni Prākrit
#> 1031                        Sauraseni Prākrit (Arabic script)
#> 1032                        Sauraseni Prākrit (Brahmi script)
#> 1033                    Sauraseni Prākrit (Devanagari script)
#> 1034                      Sauraseni Prākrit (Gurmukhi script)
#> 1035                                               Portuguese
#> 1036                 Portuguese (1990 Orthographic Agreement)
#> 1037                                     Brazilian Portuguese
#> 1038                 Portuguese (1945 Orthographic Agreement)
#> 1039                                      European Portuguese
#> 1040                                                   Paiwan
#> 1041                                              Western Pwo
#> 1042                                                   Puyuma
#> 1043                                                    Pazeh
#> 1044                                                  Quechua
#> 1045                                                  Kʼicheʼ
#> 1046                              Chimborazo Highland Quichua
#> 1047                                   Huaylas Ancash Quechua
#> 1048                                             Puno Quechua
#> 1049                                                  Qashqai
#> 1050                                                   Quenya
#> 1051                                                  Logooli
#> 1052                                                    Rabha
#> 1053                                               Rajasthani
#> 1054                                                  Rapanui
#> 1055                                               Rarotongan
#> 1056                                    Réunion Creole French
#> 1057                                                   Rejang
#> 1058                                                 Romagnol
#> 1059                                                 Rohingya
#> 1060                                 Rohingya (Arabic script)
#> 1061                        Rohingya (Hanifi Rohingya script)
#> 1062                                                  Riffian
#> 1063                                                     Raji
#> 1064                                                Arakanese
#> 1065                                                 Rangpuri
#> 1066                                                  Romansh
#> 1067                                                    Putèr
#> 1068                                       Rumantsch Grischun
#> 1069                                                 Surmiran
#> 1070                                                Sursilvan
#> 1071                                                Sutsilvan
#> 1072                                                 Vallader
#> 1073                                        Carpathian Romani
#> 1074                                             Finnish Kalo
#> 1075                                      Traveller Norwegian
#> 1076                                            Baltic Romani
#> 1077                          Baltic Romani (Cyrillic script)
#> 1078                                            Balkan Romani
#> 1079                                             Sinte Romani
#> 1080                                             Welsh-Romani
#> 1081                                              Vlax Romani
#> 1082                                                    Rundi
#> 1083                                                   Rangpo
#> 1084                                                 Romanian
#> 1085                                        Moldovan Romanian
#> 1086                                        Romance languages
#> 1087                                                Aromanian
#> 1088                                                Tarantino
#> 1089                                                    Rombo
#> 1090                                                   Romany
#> 1091                                          Pannonian Rusyn
#> 1092                                                  Rotuman
#> 1093                                                  Russian
#> 1094                            Russian (Petrine orthography)
#> 1095                                                    Rusyn
#> 1096                                                  Roviana
#> 1097                                           Istro Romanian
#> 1098                                                Aromanian
#> 1099                                         Megleno-Romanian
#> 1100                       Megleno-Romanian (Cyrillic script)
#> 1101                          Megleno-Romanian (Latin script)
#> 1102                                                    Rutul
#> 1103                                              Kinyarwanda
#> 1104                                                      Rwa
#> 1105                                          Marwari (India)
#> 1106                                                  Yaeyama
#> 1107                                Yaeyama (Hiragana script)
#> 1108                                                 Okinawan
#> 1109                               Okinawan (Hiragana script)
#> 1110                                                 Sanskrit
#> 1111                                Sanskrit (Siddham script)
#> 1112                                                  Sandawe
#> 1113                                                    Yakut
#> 1114                      South American indigenous languages
#> 1115                                       Salishan languages
#> 1116                                        Samaritan Aramaic
#> 1117                                                  Samburu
#> 1118                                                    Sasak
#> 1119                                                  Santali
#> 1120                                 Santali (Bengali script)
#> 1121                                   Santali (Latin script)
#> 1122                                   Santali (Oriya script)
#> 1123                                               Sourashtra
#> 1124                                                  Ngambay
#> 1125                                                    Sangu
#> 1126                                                Sardinian
#> 1127                                          Sri Lanka Malay
#> 1128                                                    Shina
#> 1129                                                 Sicilian
#> 1130                                                    Scots
#> 1131                                               Shetlandic
#> 1132                                                   Sindhi
#> 1133                               Sindhi (Devanagari script)
#> 1134                                 Sindhi (Gujarati script)
#> 1135                                   Sindhi (Khojki script)
#> 1136                                Sindhi (Khudawadi script)
#> 1137                                      Sassarese Sardinian
#> 1138                                         Southern Kurdish
#> 1139                         Southern Kurdish (Arabic script)
#> 1140                          Southern Kurdish (Latin script)
#> 1141                                             Bukar–Sadong
#> 1142                                            Northern Sami
#> 1143                                  Northern Sami (Finland)
#> 1144                                   Northern Sami (Norway)
#> 1145                                   Northern Sami (Sweden)
#> 1146                                                    Semai
#> 1147                                                   Seneca
#> 1148                                                     Sena
#> 1149                                                     Seri
#> 1150                                                   Selkup
#> 1151                                        Semitic languages
#> 1152                                                  Serrano
#> 1153                                          Koyraboro Senni
#> 1154                             French Belgian Sign Language
#> 1155                                                    Sango
#> 1156                                                Old Irish
#> 1157                                                  Shughni
#> 1158                                  Shughni (Arabic script)
#> 1159                                Shughni (Cyrillic script)
#> 1160                                   Shughni (Latin script)
#> 1161                                           sign languages
#> 1162                                               Samogitian
#> 1163                                Sanglechi (Arabic script)
#> 1164                                 Sanglechi (Latin script)
#> 1165                                           Serbo-Croatian
#> 1166                         Serbo-Croatian (Cyrillic script)
#> 1167                            Serbo-Croatian (Latin script)
#> 1168                                             Kundal Shahi
#> 1169                                                Tachelhit
#> 1170                                 Tachelhit (Latin script)
#> 1171                              Tachelhit (Tifinagh script)
#> 1172                                                     Shan
#> 1173                                           Chadian Arabic
#> 1174                                                  Shawiya
#> 1175                                  Shawiya (Arabic script)
#> 1176                                   Shawiya (Latin script)
#> 1177                                Shawiya (Tifinagh script)
#> 1178                                                  Sinhala
#> 1179                                              Akkala Sami
#> 1180                                                   Sidamo
#> 1181                                           Simple English
#> 1182                                         Siouan languages
#> 1183                                   Sino-Tibetan languages
#> 1184                                              Kildin Sami
#> 1185                                                Pite Sami
#> 1186                                                Kemi Sami
#> 1187                                                 Sindarin
#> 1188                                                     Xibe
#> 1189                                         Senhaja De Srair
#> 1190                                                 Ter Sami
#> 1191                                                 Ume Sami
#> 1192                                                   Slovak
#> 1193                                                  Saraiki
#> 1194                                  Saraiki (Arabic script)
#> 1195                                                Slovenian
#> 1196                                         Slavic languages
#> 1197                                     Southern Lushootseed
#> 1198                                           Lower Silesian
#> 1199                                                    Salar
#> 1200                                                  Selayar
#> 1201                                                   Samoan
#> 1202                                            Southern Sami
#> 1203                                           Sámi languages
#> 1204                                                Lule Sami
#> 1205                                               Inari Sami
#> 1206                                               Skolt Sami
#> 1207                                                    Shona
#> 1208                                                    Jagoi
#> 1209                                                  Soninke
#> 1210                                                   Somali
#> 1211                                                  Sogdien
#> 1212                                        Songhay languages
#> 1213                                               Sambalpuri
#> 1214                                                 Albanian
#> 1215                                                  Serbian
#> 1216                                Serbian (Cyrillic script)
#> 1217                                Serbian (Cyrillic script)
#> 1218                                   Serbian (Latin script)
#> 1219                                   Serbian (Latin script)
#> 1220                                              Montenegrin
#> 1221                                 Sarikoli (Arabic script)
#> 1222                               Sarikoli (Cyrillic script)
#> 1223                                  Sarikoli (Latin script)
#> 1224                                                 Serudung
#> 1225                                             Sranan Tongo
#> 1226                                    Campidanese Sardinian
#> 1227                                                  Sirionó
#> 1228                                                    Serer
#> 1229                                                    Swati
#> 1230                                   Nilo-Saharan languages
#> 1231                                            Southern Sama
#> 1232                                                     Thao
#> 1233                                                     Saho
#> 1234                                           Southern Sotho
#> 1235                                                   Shelta
#> 1236                                        Saterland Frisian
#> 1237                                           Straits Salish
#> 1238                                           Siberian Tatar
#> 1239                                                Sundanese
#> 1240                                                   Sukuma
#> 1241                                                     Susu
#> 1242                                                 Sumerian
#> 1243                                  Sumerian (Latin script)
#> 1244                              Sumerian (Cuneiform script)
#> 1245                                                   Sunwar
#> 1246                                                  Swedish
#> 1247                                                     Svan
#> 1248                                            Molise Slavic
#> 1249                                                  Swahili
#> 1250                                  Swahili (Arabic script)
#> 1251                           Swahili (Arabic script, Congo)
#> 1252                      Swahili (Arabic script, Mozambique)
#> 1253                                            Congo Swahili
#> 1254                                                 Comorian
#> 1255                                                   Saaroa
#> 1256                                              Upper Saxon
#> 1257                                         Classical Syriac
#> 1258                                                  Sylheti
#> 1259                                 Sylheti (Bengali script)
#> 1260                           Sylheti (Sylheti Nagri script)
#> 1261                                                   Syriac
#> 1262                                                 Silesian
#> 1263                                                 Sakizaya
#> 1264                                                    Tamil
#> 1265                                            Tai languages
#> 1266                                                     Yami
#> 1267                                                   Atayal
#> 1268                                                    Tboli
#> 1269                                        Southern Tutchone
#> 1270                                                     Tulu
#> 1271                                                 Tai Nuea
#> 1272                                                   Telugu
#> 1273                                                    Timne
#> 1274                                                     Teso
#> 1275                                                   Tereno
#> 1276                                                    Tetum
#> 1277                                                    Tajik
#> 1278                                  Tajik (Cyrillic script)
#> 1279                                     Tajik (Latin script)
#> 1280                                                   Tagish
#> 1281                                                     Thai
#> 1282                                            Kochila Tharu
#> 1283                                               Rana Tharu
#> 1284                                                  Tahltan
#> 1285                                                 Tigrinya
#> 1286                                                    Tigre
#> 1287                                                  Timugon
#> 1288                                                      Tiv
#> 1289                                           Northern Tujia
#> 1290                                                  Turkmen
#> 1291                                                Tokelauan
#> 1292                                                  Tsakhur
#> 1293                                                  Tagalog
#> 1294                                                   Tobelo
#> 1295                                                  Klingon
#> 1296                                   Klingon (Latin script)
#> 1297                                 Klingon (Klingon script)
#> 1298                                                  Tlingit
#> 1299                                                   Talysh
#> 1300                                 Talysh (Cyrillic script)
#> 1301                                                 Tamashek
#> 1302                                Jewish Babylonian Aramaic
#> 1303                                                   Tswana
#> 1304                                                    Taíno
#> 1305                                                   Tongan
#> 1306                                              Nyasa Tonga
#> 1307                                          Tonga (Botatwe)
#> 1308                                                Toki Pona
#> 1309                                                Tok Pisin
#> 1310                                                  Turkish
#> 1311                                                 Kokborok
#> 1312                                                   Turoyo
#> 1313                                                   Taroko
#> 1314                                                  Torwali
#> 1315                                                   Tsonga
#> 1316                                                Tsakonian
#> 1317                                                   Tausug
#> 1318                                                Tsimshian
#> 1319                                                     Tsou
#> 1320                                              Tsishingini
#> 1321                                                    Tatar
#> 1322                                  Tatar (Cyrillic script)
#> 1323                                     Tatar (Latin script)
#> 1324                                                    Tooro
#> 1325                                        Northern Tutchone
#> 1326                                               Muslim Tat
#> 1327                                                   Tupuri
#> 1328                                                  Tumbuka
#> 1329                                         Tupian languages
#> 1330                                         Altaic languages
#> 1331                                                   Tuvalu
#> 1332                                                    Tunen
#> 1333                                                      Twi
#> 1334                                                  Tweants
#> 1335                                                  Tasawaq
#> 1336                                                Tombonuwo
#> 1337                                                   Tangut
#> 1338                                    Toto (Bengali script)
#> 1339                                       Toto (Toto script)
#> 1340                                                   Tatana
#> 1341                                                 Tahitian
#> 1342                                                 Tuvinian
#> 1343                                                 Talossan
#> 1344                                  Central Atlas Tamazight
#> 1345                                                  Tzotzil
#> 1346                                                   Udmurt
#> 1347                                                   Uyghur
#> 1348                                   Uyghur (Arabic script)
#> 1349                                 Uyghur (Cyrillic script)
#> 1350                                    Uyghur (Latin script)
#> 1351                                                 Ugaritic
#> 1352                                                Ukrainian
#> 1353                                                     Ulch
#> 1354                                             Unserdeutsch
#> 1355                                                  Umbundu
#> 1356                                                   Munsee
#> 1357                                    undetermined language
#> 1358                                                  Mundari
#> 1359                              Mundari (Devanagari script)
#> 1360                             Mundari (Nag Mundari script)
#> 1361                                                    Kulon
#> 1362                                                     Urdu
#> 1363                                              Urak Lawoiʼ
#> 1364                                                   Ushoji
#> 1365                                                    Pazeh
#> 1366                                                    Uzbek
#> 1367                                  Uzbek (Cyrillic script)
#> 1368                                     Uzbek (Latin script)
#> 1369                                           Southern Uzbek
#> 1370                                                      Vai
#> 1371                                                    Venda
#> 1372                                                 Venetian
#> 1373                                                     Veps
#> 1374                                    Flemish Sign Language
#> 1375                                               Vietnamese
#> 1376                                  Vietnamese (Han script)
#> 1377                                             West Flemish
#> 1378                                          Belgian Flemish
#> 1379                                           French Flemish
#> 1380                                            Dutch Flemish
#> 1381                                          Main-Franconian
#> 1382                                                  Makhuwa
#> 1383                                                  Volapük
#> 1384                                                    Votic
#> 1385                                                     Võro
#> 1386                                                    Vunjo
#> 1387                                                     Vute
#> 1388                                                  Walloon
#> 1389                                                   Walser
#> 1390                                       Wakashan languages
#> 1391                                                 Wolaytta
#> 1392                                                    Waray
#> 1393                                                    Washo
#> 1394                                                   Wayana
#> 1395                                    Wakhi (Arabic script)
#> 1396                       Wakhi (Arabic script, Afghanistan)
#> 1397                             Wakhi (Arabic script, China)
#> 1398                          Wakhi (Arabic script, Pakistan)
#> 1399                                  Wakhi (Cyrillic script)
#> 1400                                     Wakhi (Latin script)
#> 1401                                                 Warlpiri
#> 1402                                        Sorbian languages
#> 1403                                        Pidgin (Cameroon)
#> 1404                                             Middle Welsh
#> 1405                                                Wallisian
#> 1406                                                     Wali
#> 1407                                                    Wolof
#> 1408                                           Adilabad Gondi
#> 1409                                      Wotapuri-Katarqalai
#> 1410                                                       Wu
#> 1411                               Wu (Simplified Han script)
#> 1412                              Wu (Traditional Han script)
#> 1413                                                  Wyandot
#> 1414                                               Woiwurrung
#> 1415                                                   Kalmyk
#> 1416                                            Middle Breton
#> 1417                                                    Xhosa
#> 1418                                               Mingrelian
#> 1419                                             Manado Malay
#> 1420                                               Kanakanavu
#> 1421                                             Anglo-Norman
#> 1422                                                   Kangri
#> 1423                               Kangri (Devanagari script)
#> 1424                                    Kangri (Takri script)
#> 1425                                                     Soga
#> 1426                                                 Konkomba
#> 1427                                                    Punic
#> 1428                                                   Sanumá
#> 1429                                                 Saisiyat
#> 1430                                                   Yaghan
#> 1431                             Yazghulami (Cyrillic script)
#> 1432                                Yazghulami (Latin script)
#> 1433                               Yaghnobi (Cyrillic script)
#> 1434                                  Yaghnobi (Latin script)
#> 1435                                                      Yao
#> 1436                                                   Yapese
#> 1437                                                   Nugunu
#> 1438                                                  Yambeta
#> 1439                                                  Yangben
#> 1440                                                    Yemba
#> 1441                                          Eastern Yiddish
#> 1442                                                   Yidgha
#> 1443                                                  Yeniche
#> 1444                                                  Yiddish
#> 1445                                          Tundra Yukaghir
#> 1446                                                   Yoruba
#> 1447                                                 Yonaguni
#> 1448                               Yonaguni (Hiragana script)
#> 1449                                                    Yoron
#> 1450                                  Yoron (Hiragana script)
#> 1451                                          Yupik languages
#> 1452                                                   Nenets
#> 1453                                                Nheengatu
#> 1454                                             Yucatec Maya
#> 1455                                                Cantonese
#> 1456                        Cantonese (Simplified Han script)
#> 1457                       Cantonese (Traditional Han script)
#> 1458                                                   Zhuang
#> 1459                                          Isthmus Zapotec
#> 1460                                                  Zapotec
#> 1461                                              Blissymbols
#> 1462                                                Zeelandic
#> 1463                                                   Zenaga
#> 1464                              Standard Moroccan Tamazight
#> 1465               Standard Moroccan Tamazight (Latin script)
#> 1466                                                  Chinese
#> 1467                                         Literary Chinese
#> 1468                                          Chinese (China)
#> 1469                                       Simplified Chinese
#> 1470                                      Traditional Chinese
#> 1471                                      Chinese (Hong Kong)
#> 1472                                                   Minnan
#> 1473                                          Chinese (Macau)
#> 1474                                       Chinese (Malaysia)
#> 1475                                      Chinese (Singapore)
#> 1476                                         Chinese (Taiwan)
#> 1477                                                Cantonese
#> 1478                                    Negeri Sembilan Malay
#> 1479                                          Zande languages
#> 1480                                          Yalálag Zapotec
#> 1481                                                     Zulu
#> 1482                                                     Zuni
#> 1483                                    no linguistic content
#> 1484                                                     Zaza
#>                              autonym
#> 1                           Qafár af
#> 2                          Arbërisht
#> 3                             аԥсшәа
#> 4                                   
#> 5                                   
#> 6                                   
#> 7                              Abron
#> 8                       bahasa ambon
#> 9                                   
#> 10                              Acèh
#> 11                  Kwéyòl Sent Lisi
#> 12                                  
#> 13                             عراقي
#> 14                                  
#> 15                                  
#> 16                                  
#> 17                          адыгабзэ
#> 18                          адыгабзэ
#> 19                                  
#> 20                                  
#> 21                     تونسي / Tûnsî
#> 22                             تونسي
#> 23                             Tûnsî
#> 24                                  
#> 25                                  
#> 26                                  
#> 27                         Afrikaans
#> 28                                  
#> 29                                  
#> 30                                  
#> 31                                  
#> 32                                  
#> 33          Aanteegan an' Baabyuudan
#> 34                                  
#> 35                                  
#> 36                                  
#> 37                                  
#> 38                                  
#> 39                                  
#> 40                              Akan
#> 41                                  
#> 42                                  
#> 43                                  
#> 44                                  
#> 45                                  
#> 46                                  
#> 47                                  
#> 48                                  
#> 49                                  
#> 50                              Gegë
#> 51                                  
#> 52                       Alemannisch
#> 53                         алтай тил
#> 54                                  
#> 55                              አማርኛ
#> 56                           Pangcah
#> 57                                  
#> 58                          aragonés
#> 59                                  
#> 60                           Ænglisc
#> 61                             Obolo
#> 62                             अंगिका
#> 63                                  
#> 64                              شامي
#> 65                                  
#> 66                                  
#> 67                                  
#> 68                           العربية
#> 69                                  
#> 70                             ܐܪܡܝܐ
#> 71                                  
#> 72                        mapudungun
#> 73                                  
#> 74                                  
#> 75                          جازايرية
#> 76                                  
#> 77                                  
#> 78                                  
#> 79                           الدارجة
#> 80                                  
#> 81                                  
#> 82                              مصرى
#> 83                            অসমীয়া
#> 84                                  
#> 85            American sign language
#> 86                         asturianu
#> 87                                  
#> 88                                  
#> 89                         Atikamekw
#> 90                                  
#> 91                                  
#> 92                              авар
#> 93                                  
#> 94                            Kotava
#> 95                              अवधी
#> 96                                  
#> 97                                  
#> 98                         Aymar aru
#> 99                                  
#> 100                                 
#> 101                     azərbaycanca
#> 102                                 
#> 103                                 
#> 104                                 
#> 105                           تۆرکجه
#> 106                                 
#> 107                        башҡортса
#> 108                                 
#> 109                                 
#> 110                                 
#> 111                                 
#> 112                                 
#> 113                        Basa Bali
#> 114                             ᬩᬲᬩᬮᬶ
#> 115                         Boarisch
#> 116                                 
#> 117                                 
#> 118                       žemaitėška
#> 119                                 
#> 120                       Batak Toba
#> 121                                 
#> 122                       Batak Toba
#> 123                                 
#> 124                     جهلسری بلوچی
#> 125                            wawle
#> 126                    Bikol Central
#> 127                       Bajau Sama
#> 128                       беларуская
#> 129         беларуская (тарашкевіца)
#> 130         беларуская (тарашкевіца)
#> 131                                 
#> 132                                 
#> 133                                 
#> 134                           Betawi
#> 135                                 
#> 136                                 
#> 137                                 
#> 138                                 
#> 139                                 
#> 140                                 
#> 141                                 
#> 142                                 
#> 143                                 
#> 144                                 
#> 145                                 
#> 146                        български
#> 147                         हरियाणवी
#> 148                                 
#> 149                                 
#> 150                  روچ کپتین بلوچی
#> 151                                 
#> 152                                 
#> 153                                 
#> 154                                 
#> 155                           भोजपुरी
#> 156                                 
#> 157                                 
#> 158                                 
#> 159                                 
#> 160                           भोजपुरी
#> 161                          Bislama
#> 162                                 
#> 163                                 
#> 164                           Banjar
#> 165                                 
#> 166                                 
#> 167                                 
#> 168                                 
#> 169                                 
#> 170                                 
#> 171                       ပအိုဝ်ႏဘာႏသာႏ
#> 172                                 
#> 173                                 
#> 174                       bamanankan
#> 175                            বাংলা
#> 176                                 
#> 177                                 
#> 178                                 
#> 179                                 
#> 180                                 
#> 181                            བོད་ཡིག
#> 182                        bòo pìkkà
#> 183                                 
#> 184                 বিষ্ণুপ্রিয়া মণিপুরী
#> 185                          بختیاری
#> 186                                 
#> 187                        brezhoneg
#> 188                                 
#> 189                           Bráhuí
#> 190                                 
#> 191                                 
#> 192                         bosanski
#> 193                                 
#> 194                                 
#> 195                                 
#> 196                                 
#> 197                                 
#> 198                                 
#> 199                 Batak Mandailing
#> 200                   Iriga Bicolano
#> 201                                 
#> 202                                 
#> 203                                 
#> 204                                 
#> 205                         Basa Ugi
#> 206                            ᨅᨔ ᨕᨘᨁᨗ
#> 207                                 
#> 208                                 
#> 209                                 
#> 210                           буряад
#> 211                                 
#> 212                                 
#> 213                                 
#> 214                                 
#> 215                           català
#> 216                                 
#> 217                                 
#> 218                                 
#> 219                                 
#> 220                                 
#> 221                                 
#> 222                                 
#> 223           Chavacano de Zamboanga
#> 224           Chavacano de Zamboanga
#> 225                                 
#> 226                             𑄌𑄋𑄴𑄟𑄳𑄦
#> 227                                 
#> 228           閩東語 / Mìng-dĕ̤ng-ngṳ̄
#> 229                                 
#> 230               閩東語（傳統漢字）
#> 231       Mìng-dĕ̤ng-ngṳ̄ (Bàng-uâ-cê)
#> 232                                 
#> 233                          нохчийн
#> 234                          Cebuano
#> 235                                 
#> 236                                 
#> 237                          Chamoru
#> 238                                 
#> 239                                 
#> 240                                 
#> 241                                 
#> 242                      chinuk wawa
#> 243                    Chahta anumpa
#> 244                                 
#> 245                              ᏣᎳᎩ
#> 246                  Tsetsêhestâhese
#> 247                                 
#> 248                                 
#> 249                                 
#> 250                                 
#> 251                                 
#> 252                                 
#> 253                                 
#> 254                                 
#> 255                                 
#> 256                                 
#> 257                                 
#> 258                                 
#> 259                                 
#> 260                            کوردی
#> 261                                 
#> 262                                 
#> 263                                 
#> 264                                 
#> 265                                 
#> 266                                 
#> 267                                 
#> 268                                 
#> 269                                 
#> 270                                 
#> 271                                 
#> 272                                 
#> 273                                 
#> 274                                 
#> 275                                 
#> 276                                 
#> 277                                 
#> 278                            corsu
#> 279                                 
#> 280                     ϯⲙⲉⲧⲣⲉⲙⲛ̀ⲭⲏⲙⲓ
#> 281                                 
#> 282                                 
#> 283                                 
#> 284                         Capiceño
#> 285              莆仙語 / Pó-sing-gṳ̂
#> 286                   莆仙语（简体）
#> 287                   莆仙語（繁體）
#> 288           Pó-sing-gṳ̂ (Báⁿ-uā-ci̍)
#> 289            Nēhiyawēwin / ᓀᐦᐃᔭᐍᐏᐣ
#> 290                                 
#> 291                                 
#> 292                                 
#> 293                                 
#> 294                     qırımtatarca
#> 295          къырымтатарджа (Кирилл)
#> 296             qırımtatarca (Latin)
#> 297                          tatarşa
#> 298                                 
#> 299                                 
#> 300                                 
#> 301                                 
#> 302                                 
#> 303                                 
#> 304                                 
#> 305                          čeština
#> 306                       kaszëbsczi
#> 307                                 
#> 308                                 
#> 309          словѣньскъ / ⰔⰎⰑⰂⰡⰐⰠⰔⰍⰟ
#> 310                                 
#> 311                          чӑвашла
#> 312                          Cymraeg
#> 313                            dansk
#> 314                         dagbanli
#> 315                                 
#> 316                                 
#> 317                                 
#> 318                                 
#> 319                                 
#> 320                                 
#> 321                          Deutsch
#> 322                                 
#> 323         Österreichisches Deutsch
#> 324            Schweizer Hochdeutsch
#> 325               Deutsch (Sie-Form)
#> 326                                 
#> 327                                 
#> 328                          Dagaare
#> 329                                 
#> 330                         Thuɔŋjäŋ
#> 331                           Zazaki
#> 332                                 
#> 333                                 
#> 334                                 
#> 335                      долган тыла
#> 336                                 
#> 337                                 
#> 338                                 
#> 339                                 
#> 340                                 
#> 341                                 
#> 342                                 
#> 343                                 
#> 344                                 
#> 345                                 
#> 346                                 
#> 347                     dolnoserbski
#> 348                                 
#> 349                                 
#> 350                     Kadazandusun
#> 351                                 
#> 352                            डोटेली
#> 353                            Duálá
#> 354                                 
#> 355                                 
#> 356                            ދިވެހިބަސް
#> 357                                 
#> 358                                 
#> 359                             ཇོང་ཁ
#> 360                                 
#> 361                                 
#> 362                           eʋegbe
#> 363                             Efịk
#> 364               emiliàn e rumagnòl
#> 365                                 
#> 366                                 
#> 367                                 
#> 368                         Ελληνικά
#> 369                                 
#> 370                                 
#> 371                                 
#> 372                                 
#> 373               emiliàn e rumagnòl
#> 374                          English
#> 375                                 
#> 376                 Canadian English
#> 377                                 
#> 378                                 
#> 379                  British English
#> 380                                 
#> 381                                 
#> 382                                 
#> 383                                 
#> 384                   Simple English
#> 385                                 
#> 386                                 
#> 387                                 
#> 388                        Esperanto
#> 389                                 
#> 390                                 
#> 391                                 
#> 392                          español
#> 393        español de América Latina
#> 394                                 
#> 395                 español (formal)
#> 396                                 
#> 397                                 
#> 398                                 
#> 399                                 
#> 400                            eesti
#> 401                                 
#> 402                                 
#> 403                                 
#> 404                          euskara
#> 405                                 
#> 406                        estremeñu
#> 407                                 
#> 408                            فارسی
#> 409                                 
#> 410                                 
#> 411                                 
#> 412                                 
#> 413                          mfantse
#> 414                                 
#> 415                                 
#> 416                         Fulfulde
#> 417                            suomi
#> 418                                 
#> 419                        meänkieli
#> 420                                 
#> 421                             võro
#> 422                 Na Vosa Vakaviti
#> 423                                 
#> 424                                 
#> 425                         føroyskt
#> 426                           fɔ̀ngbè
#> 427                                 
#> 428                         français
#> 429                                 
#> 430                                 
#> 431                                 
#> 432                  français cadien
#> 433                                 
#> 434                                 
#> 435                                 
#> 436                          arpetan
#> 437                       Nordfriisk
#> 438                       Oostfräisk
#> 439                                 
#> 440                                 
#> 441                                 
#> 442                           furlan
#> 443                   poor’íŋ belé’ŋ
#> 444                            Frysk
#> 445                          Gaeilge
#> 446                               Ga
#> 447                           Gagauz
#> 448                                 
#> 449                             贛語
#> 450                     赣语（简体）
#> 451                     贛語（繁體）
#> 452                                 
#> 453                                 
#> 454                                 
#> 455                                 
#> 456                                 
#> 457                                 
#> 458                                 
#> 459                                 
#> 460                  kréyòl Gwadloup
#> 461                 kriyòl gwiyannen
#> 462                         Gàidhlig
#> 463                                 
#> 464                                 
#> 465                                 
#> 466                                 
#> 467                                 
#> 468                                 
#> 469                           galego
#> 470                             на̄ни
#> 471                                 
#> 472                            گیلکی
#> 473                                 
#> 474                                 
#> 475                                 
#> 476                          Avañe'ẽ
#> 477                                 
#> 478                                 
#> 479     गोंयची कोंकणी / Gõychi Konknni
#> 480                      गोंयची कोंकणी
#> 481                   Gõychi Konknni
#> 482                                 
#> 483                 Bahasa Hulontalo
#> 484                           𐌲𐌿𐍄𐌹𐍃𐌺
#> 485                  Ghanaian Pidgin
#> 486                                 
#> 487                  Ἀρχαία ἑλληνικὴ
#> 488                                 
#> 489                      Alemannisch
#> 490                                 
#> 491                           ગુજરાતી
#> 492                       wayuunaiki
#> 493                                 
#> 494                         farefare
#> 495                           gungbe
#> 496                                 
#> 497                            Gaelg
#> 498                                 
#> 499                                 
#> 500                            Hausa
#> 501                                 
#> 502                                 
#> 503                                 
#> 504                                 
#> 505                                 
#> 506              客家語 / Hak-kâ-ngî
#> 507                   客家语（简体）
#> 508                   客家語（繁體）
#> 509          Hak-kâ-ngî (Pha̍k-fa-sṳ)
#> 510                                 
#> 511                          Hawaiʻi
#> 512                                 
#> 513                                 
#> 514                                 
#> 515                            עברית
#> 516                                 
#> 517                            हिन्दी
#> 518                                 
#> 519                                 
#> 520                       Fiji Hindi
#> 521                                 
#> 522                       Fiji Hindi
#> 523                          Ilonggo
#> 524                                 
#> 525                                 
#> 526                                 
#> 527                                 
#> 528                          kihunde
#> 529                                 
#> 530                                 
#> 531                                 
#> 532                            ہندکو
#> 533                        Hiri Motu
#> 534                                 
#> 535                               Ho
#> 536                         hrvatski
#> 537                                 
#> 538                          Hunsrik
#> 539                    hornjoserbsce
#> 540                             湘語
#> 541                                 
#> 542                                 
#> 543                   Kreyòl ayisyen
#> 544                                 
#> 545                           magyar
#> 546                  magyar (formal)
#> 547                                 
#> 548                                 
#> 549                          հայերեն
#> 550                   Արեւմտահայերէն
#> 551                       Otsiherero
#> 552                      interlingua
#> 553                        Jaku Iban
#> 554                           ibibio
#> 555                 Bahasa Indonesia
#> 556                      Interlingue
#> 557                                 
#> 558                             Igbo
#> 559                                 
#> 560                            Igala
#> 561                             ꆇꉙ
#> 562                                 
#> 563                        Iñupiatun
#> 564                           ᐃᓄᒃᑎᑐᑦ
#> 565                        inuktitut
#> 566                                 
#> 567                          Ilokano
#> 568                                 
#> 569                                 
#> 570                         гӀалгӀай
#> 571                              Ido
#> 572                                 
#> 573                                 
#> 574                         íslenska
#> 575                                 
#> 576                                 
#> 577                                 
#> 578                                 
#> 579                                 
#> 580                                 
#> 581                  medžuslovjansky
#> 582                  меджусловјанскы
#> 583                  medžuslovjansky
#> 584                         italiano
#> 585               ᐃᓄᒃᑎᑐᑦ / inuktitut
#> 586                                 
#> 587                                 
#> 588                                 
#> 589                           日本語
#> 590                                 
#> 591                                 
#> 592                                 
#> 593                                 
#> 594                                 
#> 595                                 
#> 596                           Patois
#> 597                                 
#> 598                      la .lojban.
#> 599                                 
#> 600                                 
#> 601                                 
#> 602                                 
#> 603                                 
#> 604                                 
#> 605                                 
#> 606                                 
#> 607                             jysk
#> 608                             Jawa
#> 609                               ꦗꦮ
#> 610                          ქართული
#> 611                    Qaraqalpaqsha
#> 612                        Taqbaylit
#> 613                                 
#> 614                                 
#> 615                      Karai-karai
#> 616                              Jju
#> 617                                 
#> 618                                 
#> 619                                 
#> 620                         адыгэбзэ
#> 621                         адыгэбзэ
#> 622                                 
#> 623                                 
#> 624                           Kabɩyɛ
#> 625                             Tyap
#> 626                                 
#> 627                                 
#> 628                     kabuverdianu
#> 629                                 
#> 630                                 
#> 631                                 
#> 632                                 
#> 633                                 
#> 634                            Kongo
#> 635                         Kumoring
#> 636                                 
#> 637                                 
#> 638                                 
#> 639                                 
#> 640                                 
#> 641                                 
#> 642                                 
#> 643                            کھوار
#> 644                           Gĩkũyũ
#> 645                                 
#> 646                        Kırmancki
#> 647                                 
#> 648                         Kwanyama
#> 649                            хакас
#> 650                              ဖၠုံလိက်
#> 651                          қазақша
#> 652                  قازاقشا (تٴوتە)
#> 653                  قازاقشا (جۇنگو)
#> 654                  қазақша (кирил)
#> 655              қазақша (Қазақстан)
#> 656                  qazaqşa (latın)
#> 657                qazaqşa (Türkïya)
#> 658                                 
#> 659                      kalaallisut
#> 660                                 
#> 661                                 
#> 662                                 
#> 663                                 
#> 664                                 
#> 665                         ភាសាខ្មែរ
#> 666                                 
#> 667                                 
#> 668                                 
#> 669                                 
#> 670                                 
#> 671                             ಕನ್ನಡ
#> 672                     Yerwa Kanuri
#> 673                                 
#> 674                                 
#> 675                                 
#> 676                           한국어
#> 677                                 
#> 678                                 
#> 679                                 
#> 680                           조선말
#> 681                                 
#> 682                       перем коми
#> 683                                 
#> 684                                 
#> 685                                 
#> 686                                 
#> 687                                 
#> 688                                 
#> 689                                 
#> 690                           kanuri
#> 691                 къарачай-малкъар
#> 692                             Krio
#> 693                        Kinaray-a
#> 694                           karjal
#> 695                                 
#> 696                                 
#> 697                             کٲشُر
#> 698                             کٲشُر
#> 699                             कॉशुर
#> 700                                 
#> 701                                 
#> 702                       Ripoarisch
#> 703                               စှီၤ
#> 704                                 
#> 705                            kurdî
#> 706                   کوردی (عەرەبی)
#> 707                   kurdî (latînî)
#> 708                          къумукъ
#> 709                           Kʋsaal
#> 710                                 
#> 711                             коми
#> 712                                 
#> 713                         kernowek
#> 714                                 
#> 715                                 
#> 716                                 
#> 717                                 
#> 718                                 
#> 719                         кыргызча
#> 720                                 
#> 721                                 
#> 722                           Latina
#> 723                           Ladino
#> 724                                 
#> 725                                 
#> 726                                 
#> 727                                 
#> 728                                 
#> 729                                 
#> 730                   Lëtzebuergesch
#> 731                            лакку
#> 732                                 
#> 733                                 
#> 734                                 
#> 735                            лезги
#> 736               Lingua Franca Nova
#> 737                          Luganda
#> 738                         Limburgs
#> 739                                 
#> 740                                 
#> 741                           Ligure
#> 742                                 
#> 743                                 
#> 744                         Līvõ kēļ
#> 745                      Lampung Api
#> 746                             لەکی
#> 747                      Lakȟótiyapi
#> 748                            Ladin
#> 749                                 
#> 750                                 
#> 751                                 
#> 752                                 
#> 753                                 
#> 754                          lombard
#> 755                          lingála
#> 756                                 
#> 757                              ລາວ
#> 758                                 
#> 759                                 
#> 760                                 
#> 761                           Silozi
#> 762                      لۊری شومالی
#> 763                                 
#> 764                         lietuvių
#> 765                          latgaļu
#> 766                                 
#> 767                           ciluba
#> 768                                 
#> 769                                 
#> 770                                 
#> 771                                 
#> 772                       Mizo ţawng
#> 773                                 
#> 774                                 
#> 775                      لئری دوٙمینی
#> 776                         latviešu
#> 777                             文言
#> 778                           Lazuri
#> 779                          Madhurâ
#> 780                                 
#> 781                             मगही
#> 782                            मैथिली
#> 783                                 
#> 784                                 
#> 785                                 
#> 786                                 
#> 787                  Basa Banyumasan
#> 788                                 
#> 789                                 
#> 790                                 
#> 791                                 
#> 792                                 
#> 793                          мокшень
#> 794                                 
#> 795                                 
#> 796                                 
#> 797                                 
#> 798                                 
#> 799                                 
#> 800                                 
#> 801                         Malagasy
#> 802                                 
#> 803                                 
#> 804                                 
#> 805                             Ebon
#> 806                                 
#> 807                                 
#> 808                       олык марий
#> 809                            Māori
#> 810                                 
#> 811                                 
#> 812                      Minangkabau
#> 813                                 
#> 814                                 
#> 815                                 
#> 816                                 
#> 817                                 
#> 818                       македонски
#> 819                                 
#> 820                           മലയാളം
#> 821                           монгол
#> 822                                 
#> 823                                 
#> 824                      manju gisun
#> 825                      manju gisun
#> 826                      ᠮᠠᠨᠵᡠ ᡤᡳᠰᡠᠨ
#> 827                         ꯃꯤꯇꯩ ꯂꯣꯟ
#> 828                                 
#> 829                                 
#> 830                                 
#> 831                                 
#> 832                                 
#> 833                           ဘာသာမန်
#> 834                     молдовеняскэ
#> 835                                 
#> 836                                 
#> 837                            moore
#> 838                            मराठी
#> 839                                 
#> 840                             Mara
#> 841                       кырык мары
#> 842                                 
#> 843                                 
#> 844                    Bahasa Melayu
#> 845                       بهاس ملايو
#> 846                                 
#> 847                            Malti
#> 848                                 
#> 849                   Baso Palembang
#> 850                                 
#> 851                                 
#> 852                          Mvskoke
#> 853                                 
#> 854                                 
#> 855                                 
#> 856                                 
#> 857                         Mirandés
#> 858                                 
#> 859                                 
#> 860                                 
#> 861                                 
#> 862                        မြန်မာဘာသာ
#> 863                                 
#> 864                                 
#> 865                           эрзянь
#> 866                          مازِرونی
#> 867                   Dorerin Naoero
#> 868                          Nāhuatl
#> 869                                 
#> 870              閩南語 / Bân-lâm-gí
#> 871                                 
#> 872                                 
#> 873               閩南語（傳統漢字）
#> 874           Bân-lâm-gí (Pe̍h-ōe-jī)
#> 875              Bân-lâm-gí (Tâi-lô)
#> 876                       Napulitano
#> 877                                 
#> 878                     norsk bokmål
#> 879                                 
#> 880                     Plattdüütsch
#> 881                     Nedersaksies
#> 882                            नेपाली
#> 883                        नेपाल भाषा
#> 884                        Oshiwambo
#> 885                                 
#> 886                          Li Niha
#> 887                                 
#> 888                              కొలామి
#> 889                             Niuē
#> 890                                 
#> 891                       Nederlands
#> 892                                 
#> 893                                 
#> 894                                 
#> 895           Nederlands (informeel)
#> 896                                 
#> 897                                 
#> 898                                 
#> 899                                 
#> 900                                 
#> 901                                 
#> 902                            nawdm
#> 903                    norsk nynorsk
#> 904                                 
#> 905                                 
#> 906                                 
#> 907                            norsk
#> 908                            ᨣᩤᩴᨾᩮᩬᩥᨦ
#> 909                                 
#> 910                          ногайша
#> 911                                 
#> 912                                 
#> 913                           Novial
#> 914                              ߒߞߏ
#> 915              isiNdebele seSewula
#> 916                                 
#> 917                                 
#> 918                        Nouormand
#> 919                                 
#> 920                                 
#> 921                 Sesotho sa Leboa
#> 922                                 
#> 923                                 
#> 924                             Nupe
#> 925                                 
#> 926                      Diné bizaad
#> 927                                 
#> 928                                 
#> 929                        Chi-Chewa
#> 930                                 
#> 931                       runyankore
#> 932                         Orunyoro
#> 933                           Nyunga
#> 934                                 
#> 935                                 
#> 936                          occitan
#> 937                                 
#> 938                                 
#> 939                                 
#> 940                                 
#> 941                      Ojibwemowin
#> 942                                 
#> 943                                 
#> 944                                 
#> 945                                 
#> 946                                 
#> 947                                 
#> 948                                 
#> 949                    livvinkarjala
#> 950                           Oromoo
#> 951                                 
#> 952                                 
#> 953                                 
#> 954                              ଓଡ଼ିଆ
#> 955                             ирон
#> 956                                 
#> 957                                 
#> 958                                 
#> 959                                 
#> 960                                 
#> 961                                 
#> 962                                 
#> 963                                 
#> 964                                 
#> 965                                 
#> 966                            ਪੰਜਾਬੀ
#> 967                                 
#> 968                                 
#> 969                       Pangasinan
#> 970                                 
#> 971                                 
#> 972                                 
#> 973                                 
#> 974                      Kapampangan
#> 975                                 
#> 976                       Papiamentu
#> 977               Papiamento (Aruba)
#> 978                                 
#> 979                                 
#> 980                                 
#> 981                           Picard
#> 982                                 
#> 983                                 
#> 984                            Naijá
#> 985                          Deitsch
#> 986                     Plautdietsch
#> 987                                 
#> 988                         Pälzisch
#> 989                                 
#> 990                                 
#> 991                                 
#> 992                                 
#> 993                                 
#> 994                                 
#> 995                                 
#> 996                                 
#> 997                                 
#> 998                                 
#> 999                                 
#> 1000                            पालि
#> 1001                                
#> 1002                Norfuk / Pitkern
#> 1003                                
#> 1004                                
#> 1005                                
#> 1006                                
#> 1007                                
#> 1008                          polski
#> 1009                                
#> 1010                                
#> 1011                                
#> 1012                      Piemontèis
#> 1013                          پنجابی
#> 1014                        Ποντιακά
#> 1015                                
#> 1016                                
#> 1017                           Nawat
#> 1018                                
#> 1019                                
#> 1020                                
#> 1021                                
#> 1022                       prūsiskan
#> 1023                                
#> 1024                                
#> 1025                            پښتو
#> 1026                                
#> 1027                                
#> 1028                                
#> 1029                                
#> 1030                                
#> 1031                                
#> 1032                                
#> 1033                                
#> 1034                                
#> 1035                       português
#> 1036                                
#> 1037             português do Brasil
#> 1038                                
#> 1039                                
#> 1040                      pinayuanan
#> 1041                                
#> 1042                                
#> 1043                                
#> 1044                       Runa Simi
#> 1045                                
#> 1046                      Runa shimi
#> 1047                                
#> 1048                                
#> 1049                                
#> 1050                                
#> 1051                                
#> 1052                                
#> 1053                                
#> 1054                                
#> 1055                                
#> 1056                                
#> 1057                                
#> 1058                        Rumagnôl
#> 1059                                
#> 1060                                
#> 1061                                
#> 1062                         Tarifit
#> 1063                                
#> 1064                             ရခိုင်
#> 1065                                
#> 1066                       rumantsch
#> 1067                                
#> 1068                                
#> 1069                                
#> 1070                                
#> 1071                                
#> 1072                                
#> 1073                     romaňi čhib
#> 1074                                
#> 1075                                
#> 1076                                
#> 1077                                
#> 1078                                
#> 1079                                
#> 1080                                
#> 1081                     romani čhib
#> 1082                        ikirundi
#> 1083                                
#> 1084                          română
#> 1085                                
#> 1086                                
#> 1087                     armãneashti
#> 1088                       tarandíne
#> 1089                                
#> 1090                                
#> 1091                           руски
#> 1092                                
#> 1093                         русский
#> 1094                                
#> 1095                      русиньскый
#> 1096                                
#> 1097                                
#> 1098                     armãneashti
#> 1099                        Vlăheşte
#> 1100                        Влахесте
#> 1101                        Vlăheşte
#> 1102                      мыхаӀбишды
#> 1103                    Ikinyarwanda
#> 1104                                
#> 1105                                
#> 1106                                
#> 1107                                
#> 1108                    うちなーぐち
#> 1109                                
#> 1110                           संस्कृतम्
#> 1111                                
#> 1112                                
#> 1113                       саха тыла
#> 1114                                
#> 1115                                
#> 1116                                
#> 1117                                
#> 1118                           Sasak
#> 1119                         ᱥᱟᱱᱛᱟᱲᱤ
#> 1120                                
#> 1121                                
#> 1122                                
#> 1123                                
#> 1124                                
#> 1125                                
#> 1126                           sardu
#> 1127                                
#> 1128                                
#> 1129                       sicilianu
#> 1130                           Scots
#> 1131                                
#> 1132                            سنڌي
#> 1133                                
#> 1134                                
#> 1135                                
#> 1136                                
#> 1137                       Sassaresu
#> 1138                     کوردی خوارگ
#> 1139                                
#> 1140                                
#> 1141                                
#> 1142                 davvisámegiella
#> 1143  davvisámegiella (Suoma bealde)
#> 1144 davvisámegiella (Norgga bealde)
#> 1145  davvisámegiella (Ruoŧa bealde)
#> 1146                                
#> 1147                                
#> 1148                                
#> 1149                     Cmique Itom
#> 1150                                
#> 1151                                
#> 1152                                
#> 1153                 Koyraboro Senni
#> 1154                                
#> 1155                           Sängö
#> 1156                                
#> 1157                                
#> 1158                                
#> 1159                                
#> 1160                                
#> 1161                                
#> 1162                      žemaitėška
#> 1163                                
#> 1164                                
#> 1165 srpskohrvatski / српскохрватски
#> 1166       српскохрватски (ћирилица)
#> 1167       srpskohrvatski (latinica)
#> 1168                                
#> 1169                         Taclḥit
#> 1170                         Taclḥit
#> 1171                         ⵜⴰⵛⵍⵃⵉⵜ
#> 1172                              တႆး
#> 1173                                
#> 1174                         tacawit
#> 1175                                
#> 1176                         tacawit
#> 1177                                
#> 1178                            සිංහල
#> 1179                                
#> 1180                                
#> 1181                  Simple English
#> 1182                                
#> 1183                                
#> 1184                 кӣллт са̄мь кӣлл
#> 1185                 bidumsámegiella
#> 1186                                
#> 1187                                
#> 1188                                
#> 1189                                
#> 1190                                
#> 1191                                
#> 1192                      slovenčina
#> 1193                         سرائیکی
#> 1194                         سرائیکی
#> 1195                     slovenščina
#> 1196                                
#> 1197                                
#> 1198                        Schläsch
#> 1199                                
#> 1200                                
#> 1201                    Gagana Samoa
#> 1202                   åarjelsaemien
#> 1203                                
#> 1204                                
#> 1205                     anarâškielâ
#> 1206                nuõrttsääʹmǩiõll
#> 1207                        chiShona
#> 1208                                
#> 1209                                
#> 1210                      Soomaaliga
#> 1211                                
#> 1212                                
#> 1213                                
#> 1214                           shqip
#> 1215                 српски / srpski
#> 1216               српски (ћирилица)
#> 1217               српски (ћирилица)
#> 1218               srpski (latinica)
#> 1219               srpski (latinica)
#> 1220                                
#> 1221                                
#> 1222                                
#> 1223                                
#> 1224                                
#> 1225                     Sranantongo
#> 1226               sardu campidanesu
#> 1227                                
#> 1228                                
#> 1229                         SiSwati
#> 1230                                
#> 1231                                
#> 1232                                
#> 1233                                
#> 1234                         Sesotho
#> 1235                                
#> 1236                       Seeltersk
#> 1237                                
#> 1238                      себертатар
#> 1239                           Sunda
#> 1240                                
#> 1241                                
#> 1242                                
#> 1243                                
#> 1244                                
#> 1245                                
#> 1246                         svenska
#> 1247                                
#> 1248                                
#> 1249                       Kiswahili
#> 1250                                
#> 1251                                
#> 1252                                
#> 1253                                
#> 1254                                
#> 1255                                
#> 1256                                
#> 1257                                
#> 1258                           ꠍꠤꠟꠐꠤ
#> 1259                                
#> 1260                                
#> 1261                                
#> 1262                         ślůnski
#> 1263                        Sakizaya
#> 1264                            தமிழ்
#> 1265                                
#> 1266                                
#> 1267                           Tayal
#> 1268                                
#> 1269                                
#> 1270                            ತುಳು
#> 1271                    ᥖᥭᥰ ᥖᥬᥲ ᥑᥨᥒᥰ
#> 1272                           తెలుగు
#> 1273                                
#> 1274                                
#> 1275                                
#> 1276                           tetun
#> 1277                          тоҷикӣ
#> 1278                          тоҷикӣ
#> 1279                          tojikī
#> 1280                                
#> 1281                             ไทย
#> 1282                                
#> 1283                                
#> 1284                                
#> 1285                            ትግርኛ
#> 1286                             ትግሬ
#> 1287                                
#> 1288                                
#> 1289                                
#> 1290                       Türkmençe
#> 1291                                
#> 1292                                
#> 1293                         Tagalog
#> 1294                                
#> 1295                                
#> 1296                                
#> 1297                                
#> 1298                                
#> 1299                          tolışi
#> 1300                          толыши
#> 1301                                
#> 1302                                
#> 1303                        Setswana
#> 1304                                
#> 1305                  lea faka-Tonga
#> 1306                                
#> 1307                                
#> 1308                       toki pona
#> 1309                       Tok Pisin
#> 1310                          Türkçe
#> 1311                                
#> 1312                          Ṫuroyo
#> 1313                          Seediq
#> 1314                                
#> 1315                        Xitsonga
#> 1316                                
#> 1317                                
#> 1318                                
#> 1319                                
#> 1320                                
#> 1321               татарча / tatarça
#> 1322                         татарча
#> 1323                         tatarça
#> 1324                        Orutooro
#> 1325                                
#> 1326                                
#> 1327                                
#> 1328                      chiTumbuka
#> 1329                                
#> 1330                                
#> 1331                                
#> 1332                                
#> 1333                             Twi
#> 1334                                
#> 1335                                
#> 1336                                
#> 1337                                
#> 1338                                
#> 1339                                
#> 1340                                
#> 1341                      reo tahiti
#> 1342                        тыва дыл
#> 1343                                
#> 1344                        ⵜⴰⵎⴰⵣⵉⵖⵜ
#> 1345                                
#> 1346                          удмурт
#> 1347            ئۇيغۇرچە / Uyghurche
#> 1348                        ئۇيغۇرچە
#> 1349                                
#> 1350                       Uyghurche
#> 1351                                
#> 1352                      українська
#> 1353                                
#> 1354                                
#> 1355                                
#> 1356                                
#> 1357                                
#> 1358                                
#> 1359                                
#> 1360                                
#> 1361                                
#> 1362                            اردو
#> 1363                                
#> 1364                                
#> 1365                                
#> 1366             oʻzbekcha / ўзбекча
#> 1367                         ўзбекча
#> 1368                       oʻzbekcha
#> 1369                                
#> 1370                                
#> 1371                       Tshivenda
#> 1372                          vèneto
#> 1373                     vepsän kel’
#> 1374                                
#> 1375                      Tiếng Việt
#> 1376                                
#> 1377                      West-Vlams
#> 1378                                
#> 1379                                
#> 1380                                
#> 1381                   Mainfränkisch
#> 1382                        emakhuwa
#> 1383                         Volapük
#> 1384                           Vaďďa
#> 1385                            võro
#> 1386                                
#> 1387                                
#> 1388                           walon
#> 1389                                
#> 1390                                
#> 1391                        wolaytta
#> 1392                         Winaray
#> 1393                                
#> 1394                                
#> 1395                                
#> 1396                                
#> 1397                                
#> 1398                                
#> 1399                                
#> 1400                                
#> 1401                                
#> 1402                                
#> 1403                                
#> 1404                                
#> 1405                       Fakaʻuvea
#> 1406                           waale
#> 1407                           Wolof
#> 1408                                
#> 1409                                
#> 1410                            吴语
#> 1411                    吴语（简体）
#> 1412                    吳語（正體）
#> 1413                                
#> 1414                                
#> 1415                          хальмг
#> 1416                                
#> 1417                        isiXhosa
#> 1418                       მარგალური
#> 1419                                
#> 1420                                
#> 1421                                
#> 1422                                
#> 1423                                
#> 1424                                
#> 1425                                
#> 1426                                
#> 1427                                
#> 1428                                
#> 1429                        saisiyat
#> 1430                                
#> 1431                                
#> 1432                                
#> 1433                                
#> 1434                                
#> 1435                                
#> 1436                                
#> 1437                                
#> 1438                                
#> 1439                                
#> 1440                                
#> 1441                                
#> 1442                                
#> 1443                                
#> 1444                           ייִדיש
#> 1445                                
#> 1446                          Yorùbá
#> 1447                                
#> 1448                                
#> 1449                                
#> 1450                                
#> 1451                                
#> 1452                                
#> 1453                        Nhẽẽgatú
#> 1454                     maaya t’aan
#> 1455                            粵語
#> 1456                    粵语（简体）
#> 1457                    粵語（繁體）
#> 1458                       Vahcuengh
#> 1459                                
#> 1460                                
#> 1461                                
#> 1462                          Zeêuws
#> 1463                                
#> 1464               ⵜⴰⵎⴰⵣⵉⵖⵜ ⵜⴰⵏⴰⵡⴰⵢⵜ
#> 1465               tamaziɣt tanawayt
#> 1466                            中文
#> 1467                            文言
#> 1468                中文（中国大陆）
#> 1469                    中文（简体）
#> 1470                    中文（繁體）
#> 1471                    中文（香港）
#> 1472             閩南語 / Bân-lâm-gí
#> 1473                    中文（澳門）
#> 1474                中文（马来西亚）
#> 1475                  中文（新加坡）
#> 1476                    中文（臺灣）
#> 1477                            粵語
#> 1478                                
#> 1479                                
#> 1480                                
#> 1481                         isiZulu
#> 1482                                
#> 1483                                
#> 1484                                
# }
```
