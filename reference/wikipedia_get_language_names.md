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
#> 176                   bnb
#> 177                   bnn
#> 178                   bnt
#> 179                   bny
#> 180                    bo
#> 181                   bol
#> 182                   bom
#> 183                   bpy
#> 184                   bqi
#> 185                   bqz
#> 186                    br
#> 187                   bra
#> 188                   brh
#> 189              brh-latn
#> 190                   brx
#> 191                    bs
#> 192                   bse
#> 193                   bsk
#> 194                   bss
#> 195                   btd
#> 196                   bth
#> 197                   btk
#> 198                   btm
#> 199                   bto
#> 200                   bts
#> 201                   btx
#> 202                   btz
#> 203                   bua
#> 204                   bug
#> 205              bug-bugi
#> 206                   bum
#> 207                   bvb
#> 208                   bwr
#> 209                   bxr
#> 210                   byn
#> 211                   byv
#> 212                   bzj
#> 213                   bzs
#> 214                    ca
#> 215                   cad
#> 216                   cai
#> 217                   cak
#> 218                   cal
#> 219                   car
#> 220                   cau
#> 221                   cay
#> 222                   cbk
#> 223               cbk-zam
#> 224                   cch
#> 225                   ccp
#> 226              ccp-beng
#> 227                   cdo
#> 228              cdo-hani
#> 229              cdo-hant
#> 230              cdo-latn
#> 231              cdz-beng
#> 232                    ce
#> 233                   ceb
#> 234                   cel
#> 235                   cgg
#> 236                    ch
#> 237                   chb
#> 238                   chg
#> 239                   chk
#> 240                   chm
#> 241                   chn
#> 242                   cho
#> 243                   chp
#> 244                   chr
#> 245                   chy
#> 246                   cic
#> 247                   ciw
#> 248                   cja
#> 249              cja-arab
#> 250              cja-cham
#> 251              cja-latn
#> 252                   cjm
#> 253              cjm-arab
#> 254              cjm-cham
#> 255              cjm-latn
#> 256                   cjy
#> 257              cjy-hans
#> 258              cjy-hant
#> 259                   ckb
#> 260              ckb-arab
#> 261              ckb-latn
#> 262                   cko
#> 263                   ckt
#> 264                   ckv
#> 265                   clc
#> 266                   cmc
#> 267                   cmg
#> 268    cmn-latn-cn-pinyin
#> 269    cmn-latn-tw-pinyin
#> 270  cmn-latn-tw-tongyong
#> 271  cmn-latn-tw-wadegile
#> 272                   cnh
#> 273                   cnr
#> 274              cnr-cyrl
#> 275              cnr-latn
#> 276                   cnx
#> 277                    co
#> 278                   coa
#> 279                   cop
#> 280                   cpe
#> 281                   cpf
#> 282                   cpp
#> 283                   cps
#> 284                   cpx
#> 285              cpx-hans
#> 286              cpx-hant
#> 287              cpx-latn
#> 288                    cr
#> 289               cr-cans
#> 290               cr-latn
#> 291                   crb
#> 292                   crg
#> 293                   crh
#> 294              crh-cyrl
#> 295              crh-latn
#> 296                crh-ro
#> 297                   crj
#> 298                   crk
#> 299                   crl
#> 300                   crm
#> 301                   crp
#> 302                   crr
#> 303                   crs
#> 304                    cs
#> 305                   csb
#> 306                   csw
#> 307                   ctg
#> 308                    cu
#> 309                   cus
#> 310                    cv
#> 311                    cy
#> 312                    da
#> 313                   dag
#> 314                   dak
#> 315                   dar
#> 316                   dav
#> 317                   day
#> 318                   dbj
#> 319                   ddn
#> 320                    de
#> 321               de-1901
#> 322                 de-at
#> 323                 de-ch
#> 324             de-formal
#> 325                   del
#> 326                   den
#> 327                   dga
#> 328                   dgr
#> 329                   din
#> 330                   diq
#> 331                   dje
#> 332                   djk
#> 333                   dkr
#> 334                   dlg
#> 335                   dmg
#> 336                   dmv
#> 337                   doi
#> 338              doi-arab
#> 339              doi-deva
#> 340              doi-dogr
#> 341                   dpp
#> 342                   dra
#> 343                   drg
#> 344                   dro
#> 345                   dru
#> 346                   dsb
#> 347                   dso
#> 348                   dtb
#> 349                   dtp
#> 350                   dtr
#> 351                   dty
#> 352                   dua
#> 353                   duf
#> 354                   dum
#> 355                    dv
#> 356                   dyo
#> 357                   dyu
#> 358                    dz
#> 359                   dzg
#> 360                   ebu
#> 361                    ee
#> 362                   efi
#> 363                   egl
#> 364                   egy
#> 365                   eka
#> 366                   ekp
#> 367                    el
#> 368                 el-cy
#> 369                   elm
#> 370                   elx
#> 371                   eme
#> 372                   eml
#> 373                    en
#> 374                 en-au
#> 375                 en-ca
#> 376               en-dsrt
#> 377            en-emodeng
#> 378                 en-gb
#> 379                 en-in
#> 380                 en-jm
#> 381                 en-nz
#> 382               en-shaw
#> 383             en-simple
#> 384                 en-uk
#> 385                 en-us
#> 386                   enm
#> 387                    eo
#> 388           eo-hsistemo
#> 389               eo-shaw
#> 390           eo-xsistemo
#> 391                    es
#> 392                es-419
#> 393                 es-es
#> 394             es-formal
#> 395                 es-mx
#> 396                 es-ni
#> 397                   ess
#> 398                   esu
#> 399                    et
#> 400                   eto
#> 401                   ett
#> 402                   etu
#> 403                    eu
#> 404                   ewo
#> 405                   ext
#> 406                   eya
#> 407                    fa
#> 408                fa-034
#> 409                 fa-af
#> 410                   fab
#> 411                   fan
#> 412                   fat
#> 413                   fax
#> 414                   fay
#> 415                    ff
#> 416                    fi
#> 417                   fil
#> 418                   fit
#> 419                   fiu
#> 420               fiu-vro
#> 421                    fj
#> 422                   fkv
#> 423                   fmp
#> 424                    fo
#> 425                   fon
#> 426                   fos
#> 427                    fr
#> 428                 fr-be
#> 429                 fr-ca
#> 430                 fr-ch
#> 431                   frc
#> 432                   frk
#> 433                   frm
#> 434                   fro
#> 435                   frp
#> 436                   frr
#> 437                   frs
#> 438                   fsl
#> 439                   fud
#> 440                   fuf
#> 441                   fur
#> 442                   fvr
#> 443                    fy
#> 444                    ga
#> 445                   gaa
#> 446                   gag
#> 447                   gah
#> 448                   gan
#> 449              gan-hans
#> 450              gan-hant
#> 451                   gay
#> 452                   gba
#> 453                   gbb
#> 454                   gbk
#> 455              gbk-deva
#> 456              gbk-takr
#> 457                   gbm
#> 458                   gbz
#> 459                   gcf
#> 460                   gcr
#> 461                    gd
#> 462                   gem
#> 463                   gez
#> 464                   gil
#> 465                   gju
#> 466              gju-arab
#> 467              gju-deva
#> 468                    gl
#> 469                   gld
#> 470                   glh
#> 471                   glk
#> 472                   gmh
#> 473                   gml
#> 474                   gmy
#> 475                    gn
#> 476                   gnq
#> 477                   goh
#> 478                   gom
#> 479              gom-deva
#> 480              gom-latn
#> 481                   gon
#> 482                   gor
#> 483                   got
#> 484                   gpe
#> 485                   grb
#> 486                   grc
#> 487                   gsg
#> 488                   gsw
#> 489                gsw-fr
#> 490                    gu
#> 491                   guc
#> 492                   gum
#> 493                   gur
#> 494                   guw
#> 495                   guz
#> 496                    gv
#> 497                   gwi
#> 498                   gya
#> 499                    ha
#> 500               ha-arab
#> 501               ha-latn
#> 502                 ha-ne
#> 503                   hac
#> 504                   hai
#> 505                   hak
#> 506              hak-hans
#> 507              hak-hant
#> 508              hak-latn
#> 509                   hav
#> 510                   haw
#> 511                   hax
#> 512                   haz
#> 513                   hbo
#> 514                    he
#> 515                   hea
#> 516                    hi
#> 517               hi-kthi
#> 518               hi-latn
#> 519                   hif
#> 520              hif-deva
#> 521              hif-latn
#> 522                   hil
#> 523                   him
#> 524                   hit
#> 525              hit-latn
#> 526              hit-xsux
#> 527                   hke
#> 528                   hmn
#> 529                   hne
#> 530                   hnj
#> 531                   hno
#> 532                    ho
#> 533                   hoc
#> 534              hoc-latn
#> 535                    hr
#> 536               hr-glag
#> 537                   hrx
#> 538                   hsb
#> 539                   hsn
#> 540              hsn-hans
#> 541              hsn-hant
#> 542                    ht
#> 543                   hts
#> 544                    hu
#> 545             hu-formal
#> 546                   hup
#> 547                   hur
#> 548                    hy
#> 549                   hyw
#> 550                    hz
#> 551                    ia
#> 552                   iba
#> 553                   ibb
#> 554                    id
#> 555                    ie
#> 556                   ifu
#> 557                    ig
#> 558                   igb
#> 559                   igl
#> 560                    ii
#> 561                   ijo
#> 562                    ik
#> 563              ike-cans
#> 564              ike-latn
#> 565                   ikt
#> 566                   ilo
#> 567                   inc
#> 568                   ine
#> 569                   inh
#> 570                    io
#> 571                   ira
#> 572                   iro
#> 573                    is
#> 574                   ish
#> 575              isk-arab
#> 576              isk-cyrl
#> 577              isk-latn
#> 578                   ist
#> 579                   isu
#> 580                   isv
#> 581              isv-cyrl
#> 582              isv-latn
#> 583                    it
#> 584                    iu
#> 585                   ivb
#> 586                   izh
#> 587                   izr
#> 588                    ja
#> 589               ja-hani
#> 590               ja-hira
#> 591               ja-hrkt
#> 592               ja-kana
#> 593                   jac
#> 594                   jak
#> 595                   jam
#> 596                   jax
#> 597                   jbo
#> 598                   jdt
#> 599              jdt-cyrl
#> 600                   jgo
#> 601                   jje
#> 602                   jmc
#> 603                   jpr
#> 604                   jrb
#> 605                   juk
#> 606                   jut
#> 607                    jv
#> 608               jv-java
#> 609                    ka
#> 610                   kaa
#> 611                   kab
#> 612                   kac
#> 613                   kag
#> 614                   kai
#> 615                   kaj
#> 616                   kam
#> 617                   kar
#> 618                   kaw
#> 619                   kbd
#> 620              kbd-cyrl
#> 621              kbd-latn
#> 622                   kbl
#> 623                   kbp
#> 624                   kcg
#> 625                   kck
#> 626                   kde
#> 627                   kea
#> 628                   kek
#> 629                   ken
#> 630                   ker
#> 631                   kfo
#> 632                   kfr
#> 633                    kg
#> 634                   kge
#> 635              kge-arab
#> 636                   kgg
#> 637                   kgp
#> 638                   kha
#> 639                   khi
#> 640                   kho
#> 641                   khq
#> 642                   khw
#> 643                    ki
#> 644                   kip
#> 645                   kiu
#> 646                   kix
#> 647                    kj
#> 648                   kjh
#> 649                   kjp
#> 650                    kk
#> 651               kk-arab
#> 652                 kk-cn
#> 653               kk-cyrl
#> 654                 kk-kz
#> 655               kk-latn
#> 656                 kk-tr
#> 657                   kkj
#> 658                    kl
#> 659                   kld
#> 660                   kln
#> 661                   kls
#> 662              kls-arab
#> 663              kls-latn
#> 664                    km
#> 665                   kmb
#> 666                   kmr
#> 667              kmr-arab
#> 668              kmr-latn
#> 669                   kmz
#> 670                    kn
#> 671                   knc
#> 672                   kne
#> 673                   knn
#> 674                   knq
#> 675                    ko
#> 676                 ko-cn
#> 677               ko-hani
#> 678               ko-kore
#> 679                 ko-kp
#> 680                 ko-kr
#> 681                   koi
#> 682                   kok
#> 683                   kos
#> 684                   koy
#> 685                   kpe
#> 686                   kqr
#> 687                   kqt
#> 688                   kqv
#> 689                    kr
#> 690                   krc
#> 691                   kri
#> 692                   krj
#> 693                   krl
#> 694                   kro
#> 695                   kru
#> 696                    ks
#> 697               ks-arab
#> 698               ks-deva
#> 699                   ksb
#> 700                   ksf
#> 701                   ksh
#> 702                   ksw
#> 703              ksy-beng
#> 704                    ku
#> 705               ku-arab
#> 706               ku-latn
#> 707                   kum
#> 708                   kus
#> 709                   kut
#> 710                    kv
#> 711                   kve
#> 712                    kw
#> 713                   kwk
#> 714                   kxd
#> 715                   kxi
#> 716                   kxn
#> 717                   kxv
#> 718                    ky
#> 719              kyw-beng
#> 720              kyw-deva
#> 721                    la
#> 722                   lad
#> 723              lad-hebr
#> 724              lad-latn
#> 725                   lag
#> 726                   lah
#> 727                   laj
#> 728                   lam
#> 729                    lb
#> 730                   lbe
#> 731                   lcm
#> 732                   ldn
#> 733                   lem
#> 734                   lez
#> 735                   lfn
#> 736                    lg
#> 737                    li
#> 738                 li-be
#> 739                 li-nl
#> 740                   lij
#> 741                lij-mc
#> 742                   lil
#> 743                   liv
#> 744                   ljp
#> 745                   lki
#> 746                   lkt
#> 747                   lld
#> 748                   lmn
#> 749              lmn-deva
#> 750              lmn-knda
#> 751              lmn-taml
#> 752              lmn-telu
#> 753                   lmo
#> 754                    ln
#> 755                   lns
#> 756                    lo
#> 757                   lol
#> 758                   lom
#> 759                   lou
#> 760                   loz
#> 761                   lrc
#> 762                   lsm
#> 763                    lt
#> 764                   ltg
#> 765                    lu
#> 766                   lua
#> 767                   lud
#> 768                   lui
#> 769                   lun
#> 770                   luo
#> 771                   lus
#> 772                   lut
#> 773                   luy
#> 774                   luz
#> 775                    lv
#> 776                   lzh
#> 777                   lzz
#> 778                   mad
#> 779                   maf
#> 780                   mag
#> 781                   mai
#> 782                   mak
#> 783              mak-bugi
#> 784                   man
#> 785                   map
#> 786               map-bms
#> 787                   mas
#> 788                   maw
#> 789                   mcn
#> 790                   mcp
#> 791                   mde
#> 792                   mdf
#> 793                   mdh
#> 794                   mdr
#> 795                   men
#> 796                   mer
#> 797                   mey
#> 798                   mfa
#> 799                   mfe
#> 800                    mg
#> 801                   mga
#> 802                   mgh
#> 803                   mgo
#> 804                    mh
#> 805                   mhk
#> 806                   mhn
#> 807                   mhr
#> 808                    mi
#> 809                   mic
#> 810                   mid
#> 811                   min
#> 812                   miq
#> 813                   mis
#> 814                   mix
#> 815                   mjd
#> 816              mjx-beng
#> 817                    mk
#> 818                   mkh
#> 819                    ml
#> 820                    mn
#> 821               mn-cyrl
#> 822               mn-mong
#> 823                   mnc
#> 824              mnc-latn
#> 825              mnc-mong
#> 826                   mni
#> 827              mni-beng
#> 828                   mnj
#> 829                   mno
#> 830                   mnq
#> 831                   mns
#> 832                   mnw
#> 833                    mo
#> 834                   moe
#> 835                   moh
#> 836                   mos
#> 837                    mr
#> 838               mr-modi
#> 839                   mrh
#> 840                   mrj
#> 841                   mrt
#> 842                   mrv
#> 843                    ms
#> 844               ms-arab
#> 845                   msi
#> 846                    mt
#> 847                   mua
#> 848                   mui
#> 849                   mul
#> 850                   mun
#> 851                   mus
#> 852                   mvf
#> 853                   mvi
#> 854              mvi-hira
#> 855                   mvv
#> 856                   mwl
#> 857                   mwr
#> 858                   mwv
#> 859                   mww
#> 860              mww-latn
#> 861                    my
#> 862                   mye
#> 863                   myn
#> 864                   myv
#> 865                   mzn
#> 866                    na
#> 867                   nah
#> 868                   nai
#> 869                   nan
#> 870              nan-hani
#> 871              nan-hans
#> 872              nan-hant
#> 873      nan-latn-pehoeji
#> 874        nan-latn-tailo
#> 875                   nap
#> 876                   naq
#> 877                    nb
#> 878                    nd
#> 879                   nds
#> 880                nds-nl
#> 881                    ne
#> 882                   new
#> 883                    ng
#> 884                   nge
#> 885                   nia
#> 886                   nic
#> 887                   nit
#> 888                   niu
#> 889                   njo
#> 890                    nl
#> 891                 nl-aw
#> 892                 nl-be
#> 893                 nl-cw
#> 894           nl-informal
#> 895                 nl-nl
#> 896                 nl-sr
#> 897                 nl-sx
#> 898         nl-u-sd-bebru
#> 899                   nla
#> 900                   nmg
#> 901                   nmz
#> 902                    nn
#> 903           nn-hognorsk
#> 904                   nnh
#> 905                   nnz
#> 906                    no
#> 907                   nod
#> 908              nod-thai
#> 909                   nog
#> 910                   non
#> 911              non-runr
#> 912                   nov
#> 913                   nqo
#> 914                    nr
#> 915                nrf-gg
#> 916                nrf-je
#> 917                   nrm
#> 918                   nsk
#> 919                   nsl
#> 920                   nso
#> 921                   ntd
#> 922                   nub
#> 923                   nup
#> 924                   nus
#> 925                    nv
#> 926                   nwc
#> 927                   nxm
#> 928                    ny
#> 929                   nym
#> 930                   nyn
#> 931                   nyo
#> 932                   nys
#> 933                   nzi
#> 934                   obt
#> 935                    oc
#> 936                   oco
#> 937                   odt
#> 938                   ofs
#> 939                    oj
#> 940                   ojb
#> 941                   ojc
#> 942                   ojp
#> 943              ojp-hani
#> 944              ojp-hira
#> 945                   ojs
#> 946                   ojw
#> 947                   oka
#> 948                   olo
#> 949                    om
#> 950                   oma
#> 951                   ood
#> 952                    or
#> 953                    os
#> 954                   osa
#> 955              osa-latn
#> 956                   osi
#> 957                   osx
#> 958                   ota
#> 959                   otk
#> 960                   oto
#> 961                   ovd
#> 962                   owl
#> 963                   oym
#> 964                    pa
#> 965               pa-guru
#> 966                   paa
#> 967                   pag
#> 968                   pal
#> 969              pal-phli
#> 970              pal-phlp
#> 971              pal-phlv
#> 972                   pam
#> 973                   pao
#> 974                   pap
#> 975                pap-aw
#> 976                   paq
#> 977                   pau
#> 978                   pbb
#> 979                   pcd
#> 980                pcd-be
#> 981                pcd-fr
#> 982                   pcm
#> 983                   pdc
#> 984                   pdt
#> 985                   peo
#> 986                   pfl
#> 987                   pgd
#> 988              pgd-arab
#> 989              pgd-deva
#> 990              pgd-khar
#> 991                   pgl
#> 992                   phi
#> 993                   phl
#> 994                   phn
#> 995              phn-latn
#> 996              phn-phnx
#> 997                   phr
#> 998                    pi
#> 999               pi-sidd
#> 1000                  pih
#> 1001                  pis
#> 1002                  pjt
#> 1003                  pkc
#> 1004                  pko
#> 1005                  pks
#> 1006                   pl
#> 1007                  plu
#> 1008                  plv
#> 1009                  plw
#> 1010                  pms
#> 1011                  pnb
#> 1012                  pnt
#> 1013                  pon
#> 1014                  pov
#> 1015                  ppl
#> 1016                  ppu
#> 1017                  pqm
#> 1018                  pra
#> 1019                  prc
#> 1020                  prg
#> 1021                  pro
#> 1022                  prs
#> 1023                   ps
#> 1024                ps-af
#> 1025                ps-pk
#> 1026                  psh
#> 1027                  psi
#> 1028                  psu
#> 1029             psu-arab
#> 1030             psu-brah
#> 1031             psu-deva
#> 1032             psu-guru
#> 1033                   pt
#> 1034            pt-ao1990
#> 1035                pt-br
#> 1036          pt-colb1945
#> 1037                pt-pt
#> 1038                  pwn
#> 1039                  pwo
#> 1040                  pyu
#> 1041                  pzh
#> 1042                   qu
#> 1043                  quc
#> 1044                  qug
#> 1045                  qwh
#> 1046                  qxp
#> 1047                  qxq
#> 1048                  qya
#> 1049                  rag
#> 1050                  rah
#> 1051                  raj
#> 1052                  rap
#> 1053                  rar
#> 1054                  rcf
#> 1055                  rej
#> 1056                  rgn
#> 1057                  rhg
#> 1058             rhg-arab
#> 1059             rhg-rohg
#> 1060                  rif
#> 1061                  rji
#> 1062                  rki
#> 1063                  rkt
#> 1064                   rm
#> 1065             rm-puter
#> 1066             rm-rumgr
#> 1067          rm-surmiran
#> 1068           rm-sursilv
#> 1069           rm-sutsilv
#> 1070          rm-vallader
#> 1071                  rmc
#> 1072                  rmf
#> 1073                  rmg
#> 1074                  rml
#> 1075             rml-cyrl
#> 1076                  rmn
#> 1077                  rmo
#> 1078                  rmw
#> 1079                  rmy
#> 1080                   rn
#> 1081                  rnp
#> 1082                   ro
#> 1083                ro-md
#> 1084                  roa
#> 1085              roa-rup
#> 1086             roa-tara
#> 1087                  rof
#> 1088                  rom
#> 1089                  rsk
#> 1090                  rtm
#> 1091                   ru
#> 1092          ru-petr1708
#> 1093                  rue
#> 1094                  rug
#> 1095                  ruo
#> 1096                  rup
#> 1097                  ruq
#> 1098             ruq-cyrl
#> 1099             ruq-latn
#> 1100                  rut
#> 1101                   rw
#> 1102                  rwk
#> 1103                  rwr
#> 1104                  rys
#> 1105             rys-hira
#> 1106                  ryu
#> 1107             ryu-hira
#> 1108                   sa
#> 1109              sa-sidd
#> 1110                  sad
#> 1111                  sah
#> 1112                  sai
#> 1113                  sal
#> 1114                  sam
#> 1115                  saq
#> 1116                  sas
#> 1117                  sat
#> 1118             sat-beng
#> 1119             sat-latn
#> 1120             sat-orya
#> 1121                  saz
#> 1122                  sba
#> 1123                  sbp
#> 1124                   sc
#> 1125                  sci
#> 1126                  scl
#> 1127                  scn
#> 1128                  sco
#> 1129                  scz
#> 1130                   sd
#> 1131              sd-deva
#> 1132              sd-gujr
#> 1133              sd-khoj
#> 1134              sd-sind
#> 1135                  sdc
#> 1136                  sdh
#> 1137             sdh-arab
#> 1138             sdh-latn
#> 1139                  sdo
#> 1140                   se
#> 1141                se-fi
#> 1142                se-no
#> 1143                se-se
#> 1144                  sea
#> 1145                  see
#> 1146                  seh
#> 1147                  sei
#> 1148                  sel
#> 1149                  sem
#> 1150                  ser
#> 1151                  ses
#> 1152                  sfb
#> 1153                   sg
#> 1154                  sga
#> 1155                  sgh
#> 1156             sgh-arab
#> 1157             sgh-cyrl
#> 1158             sgh-latn
#> 1159                  sgn
#> 1160                  sgs
#> 1161             sgy-arab
#> 1162             sgy-latn
#> 1163                   sh
#> 1164              sh-cyrl
#> 1165              sh-latn
#> 1166                  shd
#> 1167                  shi
#> 1168             shi-latn
#> 1169             shi-tfng
#> 1170                  shn
#> 1171                  shu
#> 1172                  shy
#> 1173             shy-arab
#> 1174             shy-latn
#> 1175             shy-tfng
#> 1176                   si
#> 1177                  sia
#> 1178                  sid
#> 1179               simple
#> 1180                  sio
#> 1181                  sit
#> 1182                  sjd
#> 1183                  sje
#> 1184                  sjk
#> 1185                  sjn
#> 1186                  sjo
#> 1187                  sjs
#> 1188                  sjt
#> 1189                  sju
#> 1190                   sk
#> 1191                  skr
#> 1192             skr-arab
#> 1193                   sl
#> 1194                  sla
#> 1195                  slh
#> 1196                  sli
#> 1197                  slr
#> 1198                  sly
#> 1199                   sm
#> 1200                  sma
#> 1201                  smi
#> 1202                  smj
#> 1203                  smn
#> 1204                  sms
#> 1205                   sn
#> 1206                  sne
#> 1207                  snk
#> 1208                   so
#> 1209                  sog
#> 1210                  son
#> 1211                  spv
#> 1212                   sq
#> 1213                   sr
#> 1214              sr-cyrl
#> 1215                sr-ec
#> 1216                sr-el
#> 1217              sr-latn
#> 1218                sr-me
#> 1219             srh-arab
#> 1220             srh-cyrl
#> 1221             srh-latn
#> 1222                  srk
#> 1223                  srn
#> 1224                  sro
#> 1225                  srq
#> 1226                  srr
#> 1227                   ss
#> 1228                  ssa
#> 1229                  ssb
#> 1230                  ssf
#> 1231                  ssy
#> 1232                   st
#> 1233                  sth
#> 1234                  stq
#> 1235                  str
#> 1236                  sty
#> 1237                   su
#> 1238                  suk
#> 1239                  sus
#> 1240                  sux
#> 1241             sux-latn
#> 1242             sux-xsux
#> 1243                  suz
#> 1244                   sv
#> 1245                  sva
#> 1246                  svm
#> 1247                   sw
#> 1248              sw-arab
#> 1249           sw-arab-cd
#> 1250           sw-arab-mz
#> 1251                sw-cd
#> 1252                  swb
#> 1253                  sxr
#> 1254                  sxu
#> 1255                  syc
#> 1256                  syl
#> 1257             syl-beng
#> 1258             syl-sylo
#> 1259                  syr
#> 1260                  szl
#> 1261                  szy
#> 1262                   ta
#> 1263                  tai
#> 1264                  tao
#> 1265                  tay
#> 1266                  tbl
#> 1267                  tce
#> 1268                  tcy
#> 1269                  tdd
#> 1270                   te
#> 1271                  tem
#> 1272                  teo
#> 1273                  ter
#> 1274                  tet
#> 1275                   tg
#> 1276              tg-cyrl
#> 1277              tg-latn
#> 1278                  tgx
#> 1279                   th
#> 1280                  thq
#> 1281                  thr
#> 1282                  tht
#> 1283                   ti
#> 1284                  tig
#> 1285                  tih
#> 1286                  tiv
#> 1287                  tji
#> 1288                   tk
#> 1289                  tkl
#> 1290                  tkr
#> 1291                   tl
#> 1292                  tlb
#> 1293                  tlh
#> 1294             tlh-latn
#> 1295             tlh-piqd
#> 1296                  tli
#> 1297                  tly
#> 1298             tly-cyrl
#> 1299                  tmh
#> 1300                  tmr
#> 1301                   tn
#> 1302                  tnq
#> 1303                   to
#> 1304                  tog
#> 1305                  toi
#> 1306                  tok
#> 1307                  tpi
#> 1308                   tr
#> 1309                  trp
#> 1310                  tru
#> 1311                  trv
#> 1312                  trw
#> 1313                   ts
#> 1314                  tsd
#> 1315                  tsg
#> 1316                  tsi
#> 1317                  tsu
#> 1318                  tsw
#> 1319                   tt
#> 1320              tt-cyrl
#> 1321              tt-latn
#> 1322                  ttj
#> 1323                  ttm
#> 1324                  ttt
#> 1325                  tui
#> 1326                  tum
#> 1327                  tup
#> 1328                  tut
#> 1329                  tvl
#> 1330                  tvu
#> 1331                   tw
#> 1332                  twd
#> 1333                  twq
#> 1334                  txa
#> 1335                  txg
#> 1336             txo-beng
#> 1337             txo-toto
#> 1338                  txx
#> 1339                   ty
#> 1340                  tyv
#> 1341                  tzl
#> 1342                  tzm
#> 1343                  tzo
#> 1344                  udm
#> 1345                   ug
#> 1346              ug-arab
#> 1347              ug-cyrl
#> 1348              ug-latn
#> 1349                  uga
#> 1350                   uk
#> 1351                  ulc
#> 1352                  uln
#> 1353                  umb
#> 1354                  umu
#> 1355                  und
#> 1356                  unr
#> 1357             unr-deva
#> 1358             unr-nagm
#> 1359                  uon
#> 1360                   ur
#> 1361                  urk
#> 1362                  ush
#> 1363                  uun
#> 1364                   uz
#> 1365              uz-cyrl
#> 1366              uz-latn
#> 1367                  uzs
#> 1368                  vai
#> 1369                   ve
#> 1370                  vec
#> 1371                  vep
#> 1372                  vgt
#> 1373                   vi
#> 1374              vi-hani
#> 1375                  vls
#> 1376               vls-be
#> 1377               vls-fr
#> 1378               vls-nl
#> 1379                  vmf
#> 1380                  vmw
#> 1381                   vo
#> 1382                  vot
#> 1383                  vro
#> 1384                  vun
#> 1385                  vut
#> 1386                   wa
#> 1387                  wae
#> 1388                  wak
#> 1389                  wal
#> 1390                  war
#> 1391                  was
#> 1392                  way
#> 1393             wbl-arab
#> 1394          wbl-arab-af
#> 1395          wbl-arab-cn
#> 1396          wbl-arab-pk
#> 1397             wbl-cyrl
#> 1398             wbl-latn
#> 1399                  wbp
#> 1400                  wen
#> 1401                  wes
#> 1402                  wlm
#> 1403                  wls
#> 1404                  wlx
#> 1405                   wo
#> 1406                  wsg
#> 1407                  wsv
#> 1408                  wuu
#> 1409             wuu-hans
#> 1410             wuu-hant
#> 1411                  wya
#> 1412                  wyi
#> 1413                  xal
#> 1414                  xbm
#> 1415                   xh
#> 1416                  xmf
#> 1417                  xmm
#> 1418                  xnb
#> 1419                  xno
#> 1420                  xnr
#> 1421             xnr-deva
#> 1422             xnr-takr
#> 1423                  xog
#> 1424                  xon
#> 1425                  xpu
#> 1426                  xsu
#> 1427                  xsy
#> 1428                  yag
#> 1429             yah-cyrl
#> 1430             yah-latn
#> 1431             yai-cyrl
#> 1432             yai-latn
#> 1433                  yao
#> 1434                  yap
#> 1435                  yas
#> 1436                  yat
#> 1437                  yav
#> 1438                  ybb
#> 1439                  ydd
#> 1440                  ydg
#> 1441                  yec
#> 1442                   yi
#> 1443                  ykg
#> 1444                   yo
#> 1445                  yoi
#> 1446             yoi-hira
#> 1447                  yox
#> 1448             yox-hira
#> 1449                  ypk
#> 1450                  yrk
#> 1451                  yrl
#> 1452                  yua
#> 1453                  yue
#> 1454             yue-hans
#> 1455             yue-hant
#> 1456                   za
#> 1457                  zai
#> 1458                  zap
#> 1459                  zbl
#> 1460                  zea
#> 1461                  zen
#> 1462                  zgh
#> 1463             zgh-latn
#> 1464                   zh
#> 1465         zh-classical
#> 1466                zh-cn
#> 1467              zh-hans
#> 1468              zh-hant
#> 1469                zh-hk
#> 1470           zh-min-nan
#> 1471                zh-mo
#> 1472                zh-my
#> 1473                zh-sg
#> 1474                zh-tw
#> 1475               zh-yue
#> 1476                  zmi
#> 1477                  znd
#> 1478                  zpu
#> 1479                   zu
#> 1480                  zun
#> 1481                  zxx
#> 1482                  zza
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
#> 176                                                    Bookan
#> 177                                                     Bunun
#> 178                                           Bantu languages
#> 179                                                   Bintulu
#> 180                                                   Tibetan
#> 181                                                      Bole
#> 182                                                     Berom
#> 183                                               Bishnupriya
#> 184                                                 Bakhtiari
#> 185                                                     Mka'a
#> 186                                                    Breton
#> 187                                                      Braj
#> 188                                                    Brahui
#> 189                                     Brahui (Latin script)
#> 190                                                      Bodo
#> 191                                                   Bosnian
#> 192                                                     Wushi
#> 193                                                Burushaski
#> 194                                                    Akoose
#> 195                                               Batak Dairi
#> 196                                                    Biatah
#> 197                                           Batak languages
#> 198                                          Batak Mandailing
#> 199                                           Rinconada Bikol
#> 200                                          Batak Simalungun
#> 201                                                Batak Karo
#> 202                                          Batak Alas-Kluet
#> 203                                                    Buriat
#> 204                                                  Buginese
#> 205                                Buginese (Buginese script)
#> 206                                                      Bulu
#> 207                                                      Bube
#> 208                                                Bura-Pabir
#> 209                                             Russia Buriat
#> 210                                                      Blin
#> 211                                                   Medumba
#> 212                                              Belize Kriol
#> 213                                   Brazilian Sign Language
#> 214                                                   Catalan
#> 215                                                     Caddo
#> 216                                    Mesoamerican languages
#> 217                                                 Kaqchikel
#> 218                                                Carolinian
#> 219                                                     Carib
#> 220                                       Caucasian languages
#> 221                                                    Cayuga
#> 222                                                 Chavacano
#> 223                                                 Chavacano
#> 224                                                     Atsam
#> 225                                                    Chakma
#> 226                                   Chakma (Bengali script)
#> 227                                                   Mindong
#> 228                                      Mindong (Han script)
#> 229                          Mindong (Traditional Han script)
#> 230                                    Mindong (Latin script)
#> 231                                     Koda (Bengali script)
#> 232                                                   Chechen
#> 233                                                   Cebuano
#> 234                                          Celtic languages
#> 235                                                     Chiga
#> 236                                                  Chamorro
#> 237                                                   Chibcha
#> 238                                                  Chagatai
#> 239                                                  Chuukese
#> 240                                                      Mari
#> 241                                            Chinook Jargon
#> 242                                                   Choctaw
#> 243                                                 Chipewyan
#> 244                                                  Cherokee
#> 245                                                  Cheyenne
#> 246                                                 Chickasaw
#> 247                                                  Chippewa
#> 248                                              Western Cham
#> 249                              Western Cham (Arabic script)
#> 250                                Western Cham (Cham script)
#> 251                               Western Cham (Latin script)
#> 252                                              Eastern Cham
#> 253                              Eastern Cham (Arabic script)
#> 254                                Eastern Cham (Cham script)
#> 255                               Eastern Cham (Latin script)
#> 256                                                       Jin
#> 257                               Jin (Simplified Han script)
#> 258                              Jin (Traditional Han script)
#> 259                                           Central Kurdish
#> 260                           Central Kurdish (Arabic script)
#> 261                            Central Kurdish (Latin script)
#> 262                                                     Anufo
#> 263                                                   Chukchi
#> 264                                                   Kavalan
#> 265                                                 Chilcotin
#> 266                                          Chamic languages
#> 267                                       Classical Mongolian
#> 268              Mandarin (Latin script, China, Hanyu Pinyin)
#> 269             Mandarin (Latin script, Taiwan, Hanyu Pinyin)
#> 270          Mandarin (Latin script, Taiwan, Tongyong Pinyin)
#> 271  Mandarin (Latin script, Taiwan, Wade-Giles romanization)
#> 272                                                Hakha-Chin
#> 273                                               Montenegrin
#> 274                             Montenegrin (Cyrillic script)
#> 275                                Montenegrin (Latin script)
#> 276                                            Middle Cornish
#> 277                                                  Corsican
#> 278                                               Cocos Malay
#> 279                                                    Coptic
#> 280                            English-based creole languages
#> 281                             French-based creole languages
#> 282                         Portuguese-based creole languages
#> 283                                                  Capiznon
#> 284                                                    Puxian
#> 285                            Puxian (Simplified Han script)
#> 286                           Puxian (Traditional Han script)
#> 287                                     Puxian (Latin script)
#> 288                                                      Cree
#> 289                      Cree (Canadian Aboriginal syllabics)
#> 290                                       Cree (Latin script)
#> 291                                              Island Carib
#> 292                                                    Michif
#> 293                                             Crimean Tatar
#> 294                           Crimean Tatar (Cyrillic script)
#> 295                              Crimean Tatar (Latin script)
#> 296                                            Dobrujan Tatar
#> 297                                        Southern East Cree
#> 298                                               Plains Cree
#> 299                                        Northern East Cree
#> 300                                                Moose Cree
#> 301                                       creoles and pidgins
#> 302                                       Carolina Algonquian
#> 303                                     Seselwa Creole French
#> 304                                                     Czech
#> 305                                                 Kashubian
#> 306                                               Swampy Cree
#> 307                                              Chittagonian
#> 308                                             Church Slavic
#> 309                                        Cushitic languages
#> 310                                                   Chuvash
#> 311                                                     Welsh
#> 312                                                    Danish
#> 313                                                   Dagbani
#> 314                                                    Dakota
#> 315                                                    Dargwa
#> 316                                                     Taita
#> 317                                      Land Dayak languages
#> 318                                                    Idaʼan
#> 319                                                     Dendi
#> 320                                                    German
#> 321                          German (traditional orthography)
#> 322                                           Austrian German
#> 323                                         Swiss High German
#> 324                                   German (formal address)
#> 325                                                  Delaware
#> 326                                                     Slave
#> 327                                          Southern Dagaare
#> 328                                                    Dogrib
#> 329                                                     Dinka
#> 330                                                     Dimli
#> 331                                                     Zarma
#> 332                                                    Ndyuka
#> 333                                                    Kuijau
#> 334                                                    Dolgan
#> 335                                        Upper Kinabatangan
#> 336                                                    Dumpas
#> 337                                                     Dogri
#> 338                                     Dogri (Arabic script)
#> 339                                 Dogri (Devanagari script)
#> 340                                      Dogri (Dogra script)
#> 341                                                     Papar
#> 342                                       Dravidian languages
#> 343                                                    Rungus
#> 344                                                 Daro-Matu
#> 345                                                     Rukai
#> 346                                             Lower Sorbian
#> 347                                                    Desiya
#> 348                                           Eastern Kadazan
#> 349                                             Central Dusun
#> 350                                                     Lotud
#> 351                                                    Doteli
#> 352                                                     Duala
#> 353                                                    Dumbea
#> 354                                              Middle Dutch
#> 355                                                    Divehi
#> 356                                                Jola-Fonyi
#> 357                                                     Dyula
#> 358                                                  Dzongkha
#> 359                                                    Dazaga
#> 360                                                      Embu
#> 361                                                       Ewe
#> 362                                                      Efik
#> 363                                        Emiliano-Romagnolo
#> 364                                          Ancient Egyptian
#> 365                                                    Ekajuk
#> 366                                                    Ekpeye
#> 367                                                     Greek
#> 368                                             Cypriot Greek
#> 369                                                     Eleme
#> 370                                                   Elamite
#> 371                                                 Emerillon
#> 372                                        Emiliano-Romagnolo
#> 373                                                   English
#> 374                                        Australian English
#> 375                                          Canadian English
#> 376                                  English (Deseret script)
#> 377                                      Early Modern English
#> 378                                           British English
#> 379                                            Indian English
#> 380                                          Jamaican English
#> 381                                       New Zealand English
#> 382                                  English (Shavian script)
#> 383                                            Simple English
#> 384                                           British English
#> 385                                          American English
#> 386                                            Middle English
#> 387                                                 Esperanto
#> 388                          Esperanto (h-system orthography)
#> 389                                Esperanto (Shavian script)
#> 390                          Esperanto (x-system orthography)
#> 391                                                   Spanish
#> 392                                    Latin American Spanish
#> 393                                          European Spanish
#> 394                                  Spanish (formal address)
#> 395                                           Mexican Spanish
#> 396                                       Spanish (Nicaragua)
#> 397                                    Central Siberian Yupik
#> 398                                             Central Yupik
#> 399                                                  Estonian
#> 400                                                      Eton
#> 401                                                  Etruscan
#> 402                                                   Ejagham
#> 403                                                    Basque
#> 404                                                    Ewondo
#> 405                                              Extremaduran
#> 406                                                      Eyak
#> 407                                                   Persian
#> 408                                      Persian (South Asia)
#> 409                                                      Dari
#> 410                                         Annobonese Creole
#> 411                                                      Fang
#> 412                                                     Fanti
#> 413                                                      Fala
#> 414                                                 Kuhmareyi
#> 415                                                      Fula
#> 416                                                   Finnish
#> 417                                                  Filipino
#> 418                                        Tornedalen Finnish
#> 419                                     Finno-Ugric languages
#> 420                                                      Võro
#> 421                                                    Fijian
#> 422                                                    Kvensk
#> 423                                                    Fe'Fe'
#> 424                                                   Faroese
#> 425                                                       Fon
#> 426                                                    Siraya
#> 427                                                    French
#> 428                                            Belgian French
#> 429                                           Canadian French
#> 430                                              Swiss French
#> 431                                              Cajun French
#> 432                                                  Frankish
#> 433                                             Middle French
#> 434                                                Old French
#> 435                                                   Arpitan
#> 436                                          Northern Frisian
#> 437                                 Eastern Frisian Low Saxon
#> 438                                      French Sign Language
#> 439                                                   Futunan
#> 440                                                     Pular
#> 441                                                  Friulian
#> 442                                                       Fur
#> 443                                           Western Frisian
#> 444                                                     Irish
#> 445                                                        Ga
#> 446                                                    Gagauz
#> 447                                                   Alekano
#> 448                                                       Gan
#> 449                               Gan (Simplified Han script)
#> 450                              Gan (Traditional Han script)
#> 451                                                      Gayo
#> 452                                                     Gbaya
#> 453                                                  Kaytetye
#> 454                                                     Gaddi
#> 455                                 Gaddi (Devanagari script)
#> 456                                      Gaddi (Takri script)
#> 457                                                  Garhwali
#> 458                                          Zoroastrian Dari
#> 459                                       Guadeloupean Creole
#> 460                                            Guianan Creole
#> 461                                           Scottish Gaelic
#> 462                                        Germanic languages
#> 463                                                      Geez
#> 464                                                Gilbertese
#> 465                                                    Gujari
#> 466                                    Gujari (Arabic script)
#> 467                                Gujari (Devanagari script)
#> 468                                                  Galician
#> 469                                                     Nanai
#> 470                                          Northwest Pashai
#> 471                                                    Gilaki
#> 472                                        Middle High German
#> 473                                         Middle Low German
#> 474                                           Mycenaean Greek
#> 475                                                   Guarani
#> 476                                                     Ganaʼ
#> 477                                           Old High German
#> 478                                              Goan Konkani
#> 479                          Goan Konkani (Devanagari script)
#> 480                               Goan Konkani (Latin script)
#> 481                                                     Gondi
#> 482                                                 Gorontalo
#> 483                                                    Gothic
#> 484                                           Ghanaian Pidgin
#> 485                                                     Grebo
#> 486                                             Ancient Greek
#> 487                                      German Sign Language
#> 488                                                 Alemannic
#> 489                                                  Alsatian
#> 490                                                  Gujarati
#> 491                                                     Wayuu
#> 492                                                 Guambiano
#> 493                                                    Frafra
#> 494                                                       Gun
#> 495                                                     Gusii
#> 496                                                      Manx
#> 497                                                  Gwichʼin
#> 498                                                     Gbaya
#> 499                                                     Hausa
#> 500                                     Hausa (Arabic script)
#> 501                                      Hausa (Latin script)
#> 502                                             Hausa (Niger)
#> 503                                                    Gurani
#> 504                                                     Haida
#> 505                                             Hakka Chinese
#> 506                             Hakka (Simplified Han script)
#> 507                            Hakka (Traditional Han script)
#> 508                                      Hakka (Latin script)
#> 509                                                      Havu
#> 510                                                  Hawaiian
#> 511                                            Southern Haida
#> 512                                                  Hazaragi
#> 513                                           Biblical Hebrew
#> 514                                                    Hebrew
#> 515                                    Northern Qiandong Miao
#> 516                                                     Hindi
#> 517                                     Hindi (Kaithi script)
#> 518                                             Hindi (Latin)
#> 519                                                Fiji Hindi
#> 520                            Fiji Hindi (Devanagari script)
#> 521                                 Fiji Hindi (Latin script)
#> 522                                                Hiligaynon
#> 523                                            Western Pahari
#> 524                                                   Hittite
#> 525                                    Hittite (Latin script)
#> 526                                Hittite (Cuneiform script)
#> 527                                                     Hunde
#> 528                                                     Hmong
#> 529                                             Chhattisgarhi
#> 530                                                Hmong Njua
#> 531                                           Northern Hindko
#> 532                                                 Hiri Motu
#> 533                                                        Ho
#> 534                                         Ho (Latin script)
#> 535                                                  Croatian
#> 536                              Croatian (Glagolitic script)
#> 537                                                   Hunsrik
#> 538                                             Upper Sorbian
#> 539                                                     Xiang
#> 540                             Xiang (Simplified Han script)
#> 541                            Xiang (Traditional Han script)
#> 542                                            Haitian Creole
#> 543                                                     Hadza
#> 544                                                 Hungarian
#> 545                                Hungarian (formal address)
#> 546                                                      Hupa
#> 547                                                Halkomelem
#> 548                                                  Armenian
#> 549                                          Western Armenian
#> 550                                                    Herero
#> 551                                               Interlingua
#> 552                                                      Iban
#> 553                                                    Ibibio
#> 554                                                Indonesian
#> 555                                               Interlingue
#> 556                                            Mayoyao Ifugao
#> 557                                                      Igbo
#> 558                                                     Ebira
#> 559                                                     Igala
#> 560                                                Sichuan Yi
#> 561                                            Ijaw languages
#> 562                                                   Inupiaq
#> 563                   Eastern Canadian (Aboriginal syllabics)
#> 564                           Eastern Canadian (Latin script)
#> 565                                Western Canadian Inuktitut
#> 566                                                     Iloko
#> 567                                      Indo-Aryan languages
#> 568                                   Indo-European languages
#> 569                                                    Ingush
#> 570                                                       Ido
#> 571                                         Iranian languages
#> 572                                       Iroquoian languages
#> 573                                                 Icelandic
#> 574                                                      Esan
#> 575                                Ishkashimi (Arabic script)
#> 576                              Ishkashimi (Cyrillic script)
#> 577                                 Ishkashimi (Latin script)
#> 578                                                   Istriot
#> 579                                                       Isu
#> 580                                               Interslavic
#> 581                             Interslavic (Cyrillic script)
#> 582                                Interslavic (Latin script)
#> 583                                                   Italian
#> 584                                                 Inuktitut
#> 585                                                    Ibatan
#> 586                                                   Ingrian
#> 587                                                     Izere
#> 588                                                  Japanese
#> 589                                   Japanese (Kanji script)
#> 590                                Japanese (Hiragana script)
#> 591                                    Japanese (Kana script)
#> 592                                Japanese (Katakana script)
#> 593                                                    Popti'
#> 594                                                     Jakun
#> 595                                   Jamaican Creole English
#> 596                                               Jambi Malay
#> 597                                                    Lojban
#> 598                                                 Judeo-Tat
#> 599                               Judeo-Tat (Cyrillic script)
#> 600                                                    Ngomba
#> 601                                                      Jeju
#> 602                                                   Machame
#> 603                                             Judeo-Persian
#> 604                                              Judeo-Arabic
#> 605                                                     Wapan
#> 606                                                    Jutish
#> 607                                                  Javanese
#> 608                                Javanese (Javanese script)
#> 609                                                  Georgian
#> 610                                               Kara-Kalpak
#> 611                                                    Kabyle
#> 612                                                    Kachin
#> 613                                                   Kajaman
#> 614                                                  Karekare
#> 615                                                       Jju
#> 616                                                     Kamba
#> 617                                         Karenic languages
#> 618                                                      Kawi
#> 619                                                 Kabardian
#> 620                               Kabardian (Cyrillic script)
#> 621                                  Kabardian (Latin script)
#> 622                                                   Kanembu
#> 623                                                    Kabiye
#> 624                                                      Tyap
#> 625                                                   Kalanga
#> 626                                                   Makonde
#> 627                                       Cape Verdean Creole
#> 628                                                  Qʼeqchiʼ
#> 629                                                   Kenyang
#> 630                                                      Kera
#> 631                                                      Koro
#> 632                                                    Kutchi
#> 633                                                     Kongo
#> 634                                                  Komering
#> 635                                  Komering (Arabic script)
#> 636                                                   Kusunda
#> 637                                                  Kaingang
#> 638                                                     Khasi
#> 639                                         Khoisan languages
#> 640                                                 Khotanese
#> 641                                              Koyra Chiini
#> 642                                                    Khowar
#> 643                                                    Kikuyu
#> 644                                               Sheshi Kham
#> 645                                                 Kirmanjki
#> 646                                         Khiamniungan Naga
#> 647                                                  Kuanyama
#> 648                                                    Khakas
#> 649                                               Eastern Pwo
#> 650                                                    Kazakh
#> 651                                    Kazakh (Arabic script)
#> 652                                            Kazakh (China)
#> 653                                  Kazakh (Cyrillic script)
#> 654                                       Kazakh (Kazakhstan)
#> 655                                     Kazakh (Latin script)
#> 656                                           Kazakh (Turkey)
#> 657                                                      Kako
#> 658                                               Kalaallisut
#> 659                                                Gamilaraay
#> 660                                                  Kalenjin
#> 661                                                   Kalasha
#> 662                                   Kalasha (Arabic script)
#> 663                                    Kalasha (Latin script)
#> 664                                                     Khmer
#> 665                                                  Kimbundu
#> 666                                          Northern Kurdish
#> 667                          Northern Kurdish (Arabic script)
#> 668                           Northern Kurdish (Latin script)
#> 669                                          Khorasani Turkic
#> 670                                                   Kannada
#> 671                                            Central Kanuri
#> 672                                                 Kankanaey
#> 673                                     Maharashtrian Konkani
#> 674                                                    Kintaq
#> 675                                                    Korean
#> 676                                            Korean (China)
#> 677                                     Korean (Hanja script)
#> 678                                     Korean (mixed script)
#> 679                                      Korean (North Korea)
#> 680                                      Korean (South Korea)
#> 681                                              Komi-Permyak
#> 682                                                   Konkani
#> 683                                                  Kosraean
#> 684                                                   Koyukon
#> 685                                                    Kpelle
#> 686                                                Kimaragang
#> 687                                       Klias River Kadazan
#> 688                                                    Okolod
#> 689                                                    Kanuri
#> 690                                           Karachay-Balkar
#> 691                                                      Krio
#> 692                                                 Kinaray-a
#> 693                                                  Karelian
#> 694                                             Kru languages
#> 695                                                    Kurukh
#> 696                                                  Kashmiri
#> 697                                  Kashmiri (Arabic script)
#> 698                              Kashmiri (Devanagari script)
#> 699                                                  Shambala
#> 700                                                     Bafia
#> 701                                                 Colognian
#> 702                                               S'gaw Karen
#> 703                              Kharia Thar (Bengali script)
#> 704                                                   Kurdish
#> 705                                   Kurdish (Arabic script)
#> 706                                    Kurdish (Latin script)
#> 707                                                     Kumyk
#> 708                                                    Kusaal
#> 709                                                   Kutenai
#> 710                                                      Komi
#> 711                                                 Kalabakan
#> 712                                                   Cornish
#> 713                                                 Kwakʼwala
#> 714                                              Brunei Malay
#> 715                                            Keningau Murut
#> 716                                                   Kanowit
#> 717                                                      Kuvi
#> 718                                                    Kyrgyz
#> 719                                  Kurmali (Bengali script)
#> 720                               Kurmali (Devanagari script)
#> 721                                                     Latin
#> 722                                                    Ladino
#> 723                                    Ladino (Hebrew script)
#> 724                                     Ladino (Latin script)
#> 725                                                     Langi
#> 726                                           Western Panjabi
#> 727                                                     Lango
#> 728                                                     Lamba
#> 729                                             Luxembourgish
#> 730                                                       Lak
#> 731                                                    Tungag
#> 732                                                    Láadan
#> 733                                                  Nomaande
#> 734                                                  Lezghian
#> 735                                        Lingua Franca Nova
#> 736                                                     Ganda
#> 737                                                Limburgish
#> 738                                        Belgian Limburgish
#> 739                                          Dutch Limburgish
#> 740                                                  Ligurian
#> 741                                                Monégasque
#> 742                                                  Lillooet
#> 743                                                  Livonian
#> 744                                               Lampung Api
#> 745                                                      Laki
#> 746                                                    Lakota
#> 747                                                     Ladin
#> 748                                                   Lambadi
#> 749                               Lambadi (Devanagari script)
#> 750                                  Lambadi (Kannada script)
#> 751                                    Lambadi (Tamil script)
#> 752                                   Lambadi (Telugu script)
#> 753                                                   Lombard
#> 754                                                   Lingala
#> 755                                                   Lamnso'
#> 756                                                       Lao
#> 757                                                     Mongo
#> 758                                                      Loma
#> 759                                          Louisiana Creole
#> 760                                                      Lozi
#> 761                                             Northern Luri
#> 762                                                    Saamia
#> 763                                                Lithuanian
#> 764                                                 Latgalian
#> 765                                              Luba-Katanga
#> 766                                                Luba-Lulua
#> 767                                                     Ludic
#> 768                                                   Luiseno
#> 769                                                     Lunda
#> 770                                                       Luo
#> 771                                                      Mizo
#> 772                                               Lushootseed
#> 773                                                     Luyia
#> 774                                             Southern Luri
#> 775                                                   Latvian
#> 776                                          Literary Chinese
#> 777                                                       Laz
#> 778                                                  Madurese
#> 779                                                      Mafa
#> 780                                                    Magahi
#> 781                                                  Maithili
#> 782                                                   Makasar
#> 783                                 Makasar (Buginese script)
#> 784                                                  Mandingo
#> 785                                    Austronesian languages
#> 786                                                Banyumasan
#> 787                                                     Masai
#> 788                                                  Mampruli
#> 789                                                     Massa
#> 790                                                      Maka
#> 791                                                      Maba
#> 792                                                    Moksha
#> 793                                              Maguindanaon
#> 794                                                    Mandar
#> 795                                                     Mende
#> 796                                                      Meru
#> 797                                                Hassaniyya
#> 798                                    Kelantan-Pattani Malay
#> 799                                                  Morisyen
#> 800                                                  Malagasy
#> 801                                              Middle Irish
#> 802                                            Makhuwa-Meetto
#> 803                                                     Metaʼ
#> 804                                               Marshallese
#> 805                                                   Mungaka
#> 806                                                   Mòcheno
#> 807                                              Eastern Mari
#> 808                                                     Māori
#> 809                                                   Mi'kmaw
#> 810                                                   Mandaic
#> 811                                               Minangkabau
#> 812                                                   Miskito
#> 813                                      unsupported language
#> 814                                                    Mixtec
#> 815                                        Northwestern Maidu
#> 816                                   Mahali (Bengali script)
#> 817                                                Macedonian
#> 818                                                 Mon-Khmer
#> 819                                                 Malayalam
#> 820                                                 Mongolian
#> 821                               Mongolian (Cyrillic script)
#> 822                              Mongolian (Mongolian script)
#> 823                                                    Manchu
#> 824                                     Manchu (Latin script)
#> 825                                 Manchu (Mongolian script)
#> 826                                                  Manipuri
#> 827                                 Manipuri (Bengali script)
#> 828                                                     Munji
#> 829                                          Manobo languages
#> 830                                                    Minriq
#> 831                                                     Mansi
#> 832                                                       Mon
#> 833                                                  Moldovan
#> 834                                                Innu-aimun
#> 835                                                    Mohawk
#> 836                                                     Mossi
#> 837                                                   Marathi
#> 838                                     Marathi (Modi script)
#> 839                                                      Mara
#> 840                                              Western Mari
#> 841                                            Marghi Central
#> 842                                                 Mangareva
#> 843                                                     Malay
#> 844                                       Malay (Jawi script)
#> 845                                               Sabah Malay
#> 846                                                   Maltese
#> 847                                                   Mundang
#> 848                                                      Musi
#> 849                                        multiple languages
#> 850                                           Munda languages
#> 851                                                  Muscogee
#> 852                                      Peripheral Mongolian
#> 853                                                    Miyako
#> 854                                  Miyako (Hiragana script)
#> 855                                                     Tagol
#> 856                                                 Mirandese
#> 857                                                   Marwari
#> 858                                                  Mentawai
#> 859                                                 Hmong Daw
#> 860                                  Hmong Daw (Latin script)
#> 861                                                   Burmese
#> 862                                                     Myene
#> 863                                           Mayan languages
#> 864                                                     Erzya
#> 865                                               Mazanderani
#> 866                                                     Nauru
#> 867                                                   Nahuatl
#> 868                     Indigenous languages of North America
#> 869                                                    Minnan
#> 870                                       Minnan (Han script)
#> 871                            Minnan (Simplified Han script)
#> 872                           Minnan (Traditional Han script)
#> 873                                        Minnan (Pe̍h-ōe-jī)
#> 874                                           Minnan (Tâi-lô)
#> 875                                                Neapolitan
#> 876                                                      Nama
#> 877                                          Norwegian Bokmål
#> 878                                             North Ndebele
#> 879                                                Low German
#> 880                                                 Low Saxon
#> 881                                                    Nepali
#> 882                                                    Newari
#> 883                                                    Ndonga
#> 884                                                    Ngémba
#> 885                                                      Nias
#> 886                                     Niger–Congo languages
#> 887                                       Southeastern Kolami
#> 888                                                    Niuean
#> 889                                                   Ao Naga
#> 890                                                     Dutch
#> 891                                              Aruban Dutch
#> 892                                             Belgian Dutch
#> 893                                           Curaçaoan Dutch
#> 894                                  Dutch (informal address)
#> 895                                         Netherlands Dutch
#> 896                                          Surinamese Dutch
#> 897                                        Sint Maarten Dutch
#> 898                                            Brussels Dutch
#> 899                                                  Ngombala
#> 900                                                    Kwasio
#> 901                                                     Nawdm
#> 902                                         Norwegian Nynorsk
#> 903                                        Norwegian Høgnorsk
#> 904                                                 Ngiemboon
#> 905                                                  Nda'Nda'
#> 906                                                 Norwegian
#> 907                                             Northern Thai
#> 908                               Northern Thai (Thai script)
#> 909                                                     Nogai
#> 910                                                 Old Norse
#> 911                                  Old Norse (Runic script)
#> 912                                                    Novial
#> 913                                                      N’Ko
#> 914                                             South Ndebele
#> 915                                               Guernésiais
#> 916                                                  Jèrriais
#> 917                                                    Norman
#> 918                                                   Naskapi
#> 919                                   Norwegian Sign Language
#> 920                                            Northern Sotho
#> 921                                            Sesayap Tidung
#> 922                                          Nubian languages
#> 923                                                      Nupe
#> 924                                                      Nuer
#> 925                                                    Navajo
#> 926                                          Classical Newari
#> 927                                                  Numidian
#> 928                                                    Nyanja
#> 929                                                  Nyamwezi
#> 930                                                  Nyankole
#> 931                                                     Nyoro
#> 932                                                   Nyungar
#> 933                                                     Nzima
#> 934                                                Old Breton
#> 935                                                   Occitan
#> 936                                               Old Cornish
#> 937                                                 Old Dutch
#> 938                                               Old Frisian
#> 939                                                    Ojibwa
#> 940                                       Northwestern Ojibwa
#> 941                                            Central Ojibwa
#> 942                                              Old Japanese
#> 943                               Old Japanese (Kanji script)
#> 944                            Old Japanese (Hiragana script)
#> 945                                                  Oji-Cree
#> 946                                            Western Ojibwa
#> 947                                                  Okanagan
#> 948                                            Livvi-Karelian
#> 949                                                     Oromo
#> 950                                               Omaha-Ponca
#> 951                                                   O'odham
#> 952                                                      Odia
#> 953                                                   Ossetic
#> 954                                                     Osage
#> 955                                      Osage (Latin script)
#> 956                                                     Osing
#> 957                                                 Old Saxon
#> 958                                           Ottoman Turkish
#> 959                                               Old Turkish
#> 960                                         Otomian languages
#> 961                                                 Elfdalian
#> 962                                                 Old Welsh
#> 963                                                   Wayampi
#> 964                                                   Punjabi
#> 965                                 Punjabi (Gurmukhi script)
#> 966                                          Papuan languages
#> 967                                                Pangasinan
#> 968                                                   Pahlavi
#> 969                    Pahlavi (Inscriptional Pahlavi script)
#> 970                          Pahlavi (Psalter Pahlavi script)
#> 971                             Pahlavi (Book Pahlavi script)
#> 972                                                  Pampanga
#> 973                                           Northern Paiute
#> 974                                                Papiamento
#> 975                                        Papiamento (Aruba)
#> 976                                                     Parya
#> 977                                                   Palauan
#> 978                                                      Páez
#> 979                                                    Picard
#> 980                                            Belgian Picard
#> 981                                             French Picard
#> 982                                           Nigerian Pidgin
#> 983                                       Pennsylvania German
#> 984                                              Plautdietsch
#> 985                                               Old Persian
#> 986                                           Palatine German
#> 987                                                  Gāndhārī
#> 988                                  Gāndhārī (Arabic script)
#> 989                              Gāndhārī (Devanagari script)
#> 990                              Gāndhārī (Kharoshthi script)
#> 991                                           Primitive Irish
#> 992                                      Philippine languages
#> 993                                                    Palula
#> 994                                                Phoenician
#> 995                                 Phoenician (Latin script)
#> 996                            Phoenician (Phoenician script)
#> 997                                            Pahari-Potwari
#> 998                                                      Pali
#> 999                                     Pali (Siddham script)
#> 1000                                         Pitcairn-Norfolk
#> 1001                                                    Pijin
#> 1002                                           Pitjantjatjara
#> 1003                                                  Paekche
#> 1004                                                   Pökoot
#> 1005                                   Pakistan Sign Language
#> 1006                                                   Polish
#> 1007                                                  Palikur
#> 1008                                       Southwest Palawano
#> 1009                                  Brooke's Point Palawano
#> 1010                                              Piedmontese
#> 1011                                          Western Punjabi
#> 1012                                                   Pontic
#> 1013                                                Pohnpeian
#> 1014                                     Upper Guinea Crioulo
#> 1015                                                    Nawat
#> 1016                                            Papora-Hoanya
#> 1017                                   Maliseet-Passamaquoddy
#> 1018                                                  Prakrit
#> 1019                                                  Parachi
#> 1020                                                 Prussian
#> 1021                                            Old Provençal
#> 1022                                                     Dari
#> 1023                                                   Pashto
#> 1024                                     Pashto (Afghanistan)
#> 1025                                        Pashto (Pakistan)
#> 1026                                         Southwest Pashai
#> 1027                                         Southeast Pashai
#> 1028                                        Sauraseni Prākrit
#> 1029                        Sauraseni Prākrit (Arabic script)
#> 1030                        Sauraseni Prākrit (Brahmi script)
#> 1031                    Sauraseni Prākrit (Devanagari script)
#> 1032                      Sauraseni Prākrit (Gurmukhi script)
#> 1033                                               Portuguese
#> 1034                 Portuguese (1990 Orthographic Agreement)
#> 1035                                     Brazilian Portuguese
#> 1036                 Portuguese (1945 Orthographic Agreement)
#> 1037                                      European Portuguese
#> 1038                                                   Paiwan
#> 1039                                              Western Pwo
#> 1040                                                   Puyuma
#> 1041                                                    Pazeh
#> 1042                                                  Quechua
#> 1043                                                  Kʼicheʼ
#> 1044                              Chimborazo Highland Quichua
#> 1045                                   Huaylas Ancash Quechua
#> 1046                                             Puno Quechua
#> 1047                                                  Qashqai
#> 1048                                                   Quenya
#> 1049                                                  Logooli
#> 1050                                                    Rabha
#> 1051                                               Rajasthani
#> 1052                                                  Rapanui
#> 1053                                               Rarotongan
#> 1054                                    Réunion Creole French
#> 1055                                                   Rejang
#> 1056                                                 Romagnol
#> 1057                                                 Rohingya
#> 1058                                 Rohingya (Arabic script)
#> 1059                        Rohingya (Hanifi Rohingya script)
#> 1060                                                  Riffian
#> 1061                                                     Raji
#> 1062                                                Arakanese
#> 1063                                                 Rangpuri
#> 1064                                                  Romansh
#> 1065                                                    Putèr
#> 1066                                       Rumantsch Grischun
#> 1067                                                 Surmiran
#> 1068                                                Sursilvan
#> 1069                                                Sutsilvan
#> 1070                                                 Vallader
#> 1071                                        Carpathian Romani
#> 1072                                             Finnish Kalo
#> 1073                                      Traveller Norwegian
#> 1074                                            Baltic Romani
#> 1075                          Baltic Romani (Cyrillic script)
#> 1076                                            Balkan Romani
#> 1077                                             Sinte Romani
#> 1078                                             Welsh-Romani
#> 1079                                              Vlax Romani
#> 1080                                                    Rundi
#> 1081                                                   Rangpo
#> 1082                                                 Romanian
#> 1083                                                Moldavian
#> 1084                                        Romance languages
#> 1085                                                Aromanian
#> 1086                                                Tarantino
#> 1087                                                    Rombo
#> 1088                                                   Romany
#> 1089                                          Pannonian Rusyn
#> 1090                                                  Rotuman
#> 1091                                                  Russian
#> 1092                            Russian (Petrine orthography)
#> 1093                                                    Rusyn
#> 1094                                                  Roviana
#> 1095                                           Istro Romanian
#> 1096                                                Aromanian
#> 1097                                         Megleno-Romanian
#> 1098                       Megleno-Romanian (Cyrillic script)
#> 1099                          Megleno-Romanian (Latin script)
#> 1100                                                    Rutul
#> 1101                                              Kinyarwanda
#> 1102                                                      Rwa
#> 1103                                          Marwari (India)
#> 1104                                                  Yaeyama
#> 1105                                Yaeyama (Hiragana script)
#> 1106                                                 Okinawan
#> 1107                               Okinawan (Hiragana script)
#> 1108                                                 Sanskrit
#> 1109                                Sanskrit (Siddham script)
#> 1110                                                  Sandawe
#> 1111                                                    Yakut
#> 1112                      South American indigenous languages
#> 1113                                       Salishan languages
#> 1114                                        Samaritan Aramaic
#> 1115                                                  Samburu
#> 1116                                                    Sasak
#> 1117                                                  Santali
#> 1118                                 Santali (Bengali script)
#> 1119                                   Santali (Latin script)
#> 1120                                   Santali (Oriya script)
#> 1121                                               Sourashtra
#> 1122                                                  Ngambay
#> 1123                                                    Sangu
#> 1124                                                Sardinian
#> 1125                                          Sri Lanka Malay
#> 1126                                                    Shina
#> 1127                                                 Sicilian
#> 1128                                                    Scots
#> 1129                                               Shetlandic
#> 1130                                                   Sindhi
#> 1131                               Sindhi (Devanagari script)
#> 1132                                 Sindhi (Gujarati script)
#> 1133                                   Sindhi (Khojki script)
#> 1134                                Sindhi (Khudawadi script)
#> 1135                                      Sassarese Sardinian
#> 1136                                         Southern Kurdish
#> 1137                         Southern Kurdish (Arabic script)
#> 1138                          Southern Kurdish (Latin script)
#> 1139                                             Bukar–Sadong
#> 1140                                            Northern Sami
#> 1141                                  Northern Sami (Finland)
#> 1142                                   Northern Sami (Norway)
#> 1143                                   Northern Sami (Sweden)
#> 1144                                                    Semai
#> 1145                                                   Seneca
#> 1146                                                     Sena
#> 1147                                                     Seri
#> 1148                                                   Selkup
#> 1149                                        Semitic languages
#> 1150                                                  Serrano
#> 1151                                          Koyraboro Senni
#> 1152                             French Belgian Sign Language
#> 1153                                                    Sango
#> 1154                                                Old Irish
#> 1155                                                  Shughni
#> 1156                                  Shughni (Arabic script)
#> 1157                                Shughni (Cyrillic script)
#> 1158                                   Shughni (Latin script)
#> 1159                                           sign languages
#> 1160                                               Samogitian
#> 1161                                Sanglechi (Arabic script)
#> 1162                                 Sanglechi (Latin script)
#> 1163                                           Serbo-Croatian
#> 1164                         Serbo-Croatian (Cyrillic script)
#> 1165                            Serbo-Croatian (Latin script)
#> 1166                                             Kundal Shahi
#> 1167                                                Tachelhit
#> 1168                                 Tachelhit (Latin script)
#> 1169                              Tachelhit (Tifinagh script)
#> 1170                                                     Shan
#> 1171                                           Chadian Arabic
#> 1172                                                  Shawiya
#> 1173                                  Shawiya (Arabic script)
#> 1174                                   Shawiya (Latin script)
#> 1175                                Shawiya (Tifinagh script)
#> 1176                                                  Sinhala
#> 1177                                              Akkala Sami
#> 1178                                                   Sidamo
#> 1179                                           Simple English
#> 1180                                         Siouan languages
#> 1181                                   Sino-Tibetan languages
#> 1182                                              Kildin Sami
#> 1183                                                Pite Sami
#> 1184                                                Kemi Sami
#> 1185                                                 Sindarin
#> 1186                                                     Xibe
#> 1187                                         Senhaja De Srair
#> 1188                                                 Ter Sami
#> 1189                                                 Ume Sami
#> 1190                                                   Slovak
#> 1191                                                  Saraiki
#> 1192                                  Saraiki (Arabic script)
#> 1193                                                Slovenian
#> 1194                                         Slavic languages
#> 1195                                     Southern Lushootseed
#> 1196                                           Lower Silesian
#> 1197                                                    Salar
#> 1198                                                  Selayar
#> 1199                                                   Samoan
#> 1200                                            Southern Sami
#> 1201                                           Sámi languages
#> 1202                                                Lule Sami
#> 1203                                               Inari Sami
#> 1204                                               Skolt Sami
#> 1205                                                    Shona
#> 1206                                                    Jagoi
#> 1207                                                  Soninke
#> 1208                                                   Somali
#> 1209                                                  Sogdien
#> 1210                                        Songhay languages
#> 1211                                               Sambalpuri
#> 1212                                                 Albanian
#> 1213                                                  Serbian
#> 1214                                Serbian (Cyrillic script)
#> 1215                                Serbian (Cyrillic script)
#> 1216                                   Serbian (Latin script)
#> 1217                                   Serbian (Latin script)
#> 1218                                              Montenegrin
#> 1219                                 Sarikoli (Arabic script)
#> 1220                               Sarikoli (Cyrillic script)
#> 1221                                  Sarikoli (Latin script)
#> 1222                                                 Serudung
#> 1223                                             Sranan Tongo
#> 1224                                    Campidanese Sardinian
#> 1225                                                  Sirionó
#> 1226                                                    Serer
#> 1227                                                    Swati
#> 1228                                   Nilo-Saharan languages
#> 1229                                            Southern Sama
#> 1230                                                     Thao
#> 1231                                                     Saho
#> 1232                                           Southern Sotho
#> 1233                                                   Shelta
#> 1234                                        Saterland Frisian
#> 1235                                           Straits Salish
#> 1236                                           Siberian Tatar
#> 1237                                                Sundanese
#> 1238                                                   Sukuma
#> 1239                                                     Susu
#> 1240                                                 Sumerian
#> 1241                                  Sumerian (Latin script)
#> 1242                              Sumerian (Cuneiform script)
#> 1243                                                   Sunwar
#> 1244                                                  Swedish
#> 1245                                                     Svan
#> 1246                                            Molise Slavic
#> 1247                                                  Swahili
#> 1248                                  Swahili (Arabic script)
#> 1249                           Swahili (Arabic script, Congo)
#> 1250                      Swahili (Arabic script, Mozambique)
#> 1251                                            Congo Swahili
#> 1252                                                 Comorian
#> 1253                                                   Saaroa
#> 1254                                              Upper Saxon
#> 1255                                         Classical Syriac
#> 1256                                                  Sylheti
#> 1257                                 Sylheti (Bengali script)
#> 1258                           Sylheti (Sylheti Nagri script)
#> 1259                                                   Syriac
#> 1260                                                 Silesian
#> 1261                                                 Sakizaya
#> 1262                                                    Tamil
#> 1263                                            Tai languages
#> 1264                                                     Yami
#> 1265                                                   Atayal
#> 1266                                                    Tboli
#> 1267                                        Southern Tutchone
#> 1268                                                     Tulu
#> 1269                                                 Tai Nuea
#> 1270                                                   Telugu
#> 1271                                                    Timne
#> 1272                                                     Teso
#> 1273                                                   Tereno
#> 1274                                                    Tetum
#> 1275                                                    Tajik
#> 1276                                  Tajik (Cyrillic script)
#> 1277                                     Tajik (Latin script)
#> 1278                                                   Tagish
#> 1279                                                     Thai
#> 1280                                            Kochila Tharu
#> 1281                                               Rana Tharu
#> 1282                                                  Tahltan
#> 1283                                                 Tigrinya
#> 1284                                                    Tigre
#> 1285                                                  Timugon
#> 1286                                                      Tiv
#> 1287                                           Northern Tujia
#> 1288                                                  Turkmen
#> 1289                                                Tokelauan
#> 1290                                                  Tsakhur
#> 1291                                                  Tagalog
#> 1292                                                   Tobelo
#> 1293                                                  Klingon
#> 1294                                   Klingon (Latin script)
#> 1295                                 Klingon (Klingon script)
#> 1296                                                  Tlingit
#> 1297                                                   Talysh
#> 1298                                 Talysh (Cyrillic script)
#> 1299                                                 Tamashek
#> 1300                                Jewish Babylonian Aramaic
#> 1301                                                   Tswana
#> 1302                                                    Taíno
#> 1303                                                   Tongan
#> 1304                                              Nyasa Tonga
#> 1305                                          Tonga (Botatwe)
#> 1306                                                Toki Pona
#> 1307                                                Tok Pisin
#> 1308                                                  Turkish
#> 1309                                                 Kokborok
#> 1310                                                   Turoyo
#> 1311                                                   Taroko
#> 1312                                                  Torwali
#> 1313                                                   Tsonga
#> 1314                                                Tsakonian
#> 1315                                                   Tausug
#> 1316                                                Tsimshian
#> 1317                                                     Tsou
#> 1318                                              Tsishingini
#> 1319                                                    Tatar
#> 1320                                  Tatar (Cyrillic script)
#> 1321                                     Tatar (Latin script)
#> 1322                                                    Tooro
#> 1323                                        Northern Tutchone
#> 1324                                               Muslim Tat
#> 1325                                                   Tupuri
#> 1326                                                  Tumbuka
#> 1327                                         Tupian languages
#> 1328                                         Altaic languages
#> 1329                                                   Tuvalu
#> 1330                                                    Tunen
#> 1331                                                      Twi
#> 1332                                                  Tweants
#> 1333                                                  Tasawaq
#> 1334                                                Tombonuwo
#> 1335                                                   Tangut
#> 1336                                    Toto (Bengali script)
#> 1337                                       Toto (Toto script)
#> 1338                                                   Tatana
#> 1339                                                 Tahitian
#> 1340                                                 Tuvinian
#> 1341                                                 Talossan
#> 1342                                  Central Atlas Tamazight
#> 1343                                                  Tzotzil
#> 1344                                                   Udmurt
#> 1345                                                   Uyghur
#> 1346                                   Uyghur (Arabic script)
#> 1347                                 Uyghur (Cyrillic script)
#> 1348                                    Uyghur (Latin script)
#> 1349                                                 Ugaritic
#> 1350                                                Ukrainian
#> 1351                                                     Ulch
#> 1352                                             Unserdeutsch
#> 1353                                                  Umbundu
#> 1354                                                   Munsee
#> 1355                                    undetermined language
#> 1356                                                  Mundari
#> 1357                              Mundari (Devanagari script)
#> 1358                             Mundari (Nag Mundari script)
#> 1359                                                    Kulon
#> 1360                                                     Urdu
#> 1361                                              Urak Lawoiʼ
#> 1362                                                   Ushoji
#> 1363                                                    Pazeh
#> 1364                                                    Uzbek
#> 1365                                  Uzbek (Cyrillic script)
#> 1366                                     Uzbek (Latin script)
#> 1367                                           Southern Uzbek
#> 1368                                                      Vai
#> 1369                                                    Venda
#> 1370                                                 Venetian
#> 1371                                                     Veps
#> 1372                                    Flemish Sign Language
#> 1373                                               Vietnamese
#> 1374                                  Vietnamese (Han script)
#> 1375                                             West Flemish
#> 1376                                          Belgian Flemish
#> 1377                                           French Flemish
#> 1378                                            Dutch Flemish
#> 1379                                          Main-Franconian
#> 1380                                                  Makhuwa
#> 1381                                                  Volapük
#> 1382                                                    Votic
#> 1383                                                     Võro
#> 1384                                                    Vunjo
#> 1385                                                     Vute
#> 1386                                                  Walloon
#> 1387                                                   Walser
#> 1388                                       Wakashan languages
#> 1389                                                 Wolaytta
#> 1390                                                    Waray
#> 1391                                                    Washo
#> 1392                                                   Wayana
#> 1393                                    Wakhi (Arabic script)
#> 1394                       Wakhi (Arabic script, Afghanistan)
#> 1395                             Wakhi (Arabic script, China)
#> 1396                          Wakhi (Arabic script, Pakistan)
#> 1397                                  Wakhi (Cyrillic script)
#> 1398                                     Wakhi (Latin script)
#> 1399                                                 Warlpiri
#> 1400                                        Sorbian languages
#> 1401                                        Pidgin (Cameroon)
#> 1402                                             Middle Welsh
#> 1403                                                Wallisian
#> 1404                                                     Wali
#> 1405                                                    Wolof
#> 1406                                           Adilabad Gondi
#> 1407                                      Wotapuri-Katarqalai
#> 1408                                                       Wu
#> 1409                               Wu (Simplified Han script)
#> 1410                              Wu (Traditional Han script)
#> 1411                                                  Wyandot
#> 1412                                               Woiwurrung
#> 1413                                                   Kalmyk
#> 1414                                            Middle Breton
#> 1415                                                    Xhosa
#> 1416                                               Mingrelian
#> 1417                                             Manado Malay
#> 1418                                               Kanakanavu
#> 1419                                             Anglo-Norman
#> 1420                                                   Kangri
#> 1421                               Kangri (Devanagari script)
#> 1422                                    Kangri (Takri script)
#> 1423                                                     Soga
#> 1424                                                 Konkomba
#> 1425                                                    Punic
#> 1426                                                   Sanumá
#> 1427                                                 Saisiyat
#> 1428                                                   Yaghan
#> 1429                             Yazghulami (Cyrillic script)
#> 1430                                Yazghulami (Latin script)
#> 1431                               Yaghnobi (Cyrillic script)
#> 1432                                  Yaghnobi (Latin script)
#> 1433                                                      Yao
#> 1434                                                   Yapese
#> 1435                                                   Nugunu
#> 1436                                                  Yambeta
#> 1437                                                  Yangben
#> 1438                                                    Yemba
#> 1439                                          Eastern Yiddish
#> 1440                                                   Yidgha
#> 1441                                                  Yeniche
#> 1442                                                  Yiddish
#> 1443                                          Tundra Yukaghir
#> 1444                                                   Yoruba
#> 1445                                                 Yonaguni
#> 1446                               Yonaguni (Hiragana script)
#> 1447                                                    Yoron
#> 1448                                  Yoron (Hiragana script)
#> 1449                                          Yupik languages
#> 1450                                                   Nenets
#> 1451                                                Nheengatu
#> 1452                                             Yucatec Maya
#> 1453                                                Cantonese
#> 1454                        Cantonese (Simplified Han script)
#> 1455                       Cantonese (Traditional Han script)
#> 1456                                                   Zhuang
#> 1457                                          Isthmus Zapotec
#> 1458                                                  Zapotec
#> 1459                                              Blissymbols
#> 1460                                                Zeelandic
#> 1461                                                   Zenaga
#> 1462                              Standard Moroccan Tamazight
#> 1463               Standard Moroccan Tamazight (Latin script)
#> 1464                                                  Chinese
#> 1465                                         Literary Chinese
#> 1466                                          Chinese (China)
#> 1467                                       Simplified Chinese
#> 1468                                      Traditional Chinese
#> 1469                                      Chinese (Hong Kong)
#> 1470                                                   Minnan
#> 1471                                          Chinese (Macau)
#> 1472                                       Chinese (Malaysia)
#> 1473                                      Chinese (Singapore)
#> 1474                                         Chinese (Taiwan)
#> 1475                                                Cantonese
#> 1476                                    Negeri Sembilan Malay
#> 1477                                          Zande languages
#> 1478                                          Yalálag Zapotec
#> 1479                                                     Zulu
#> 1480                                                     Zuni
#> 1481                                    no linguistic content
#> 1482                                                     Zaza
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
#> 180                            བོད་ཡིག
#> 181                        bòo pìkkà
#> 182                                 
#> 183                 বিষ্ণুপ্রিয়া মণিপুরী
#> 184                          بختیاری
#> 185                                 
#> 186                        brezhoneg
#> 187                                 
#> 188                           Bráhuí
#> 189                                 
#> 190                                 
#> 191                         bosanski
#> 192                                 
#> 193                                 
#> 194                                 
#> 195                                 
#> 196                                 
#> 197                                 
#> 198                 Batak Mandailing
#> 199                   Iriga Bicolano
#> 200                                 
#> 201                                 
#> 202                                 
#> 203                                 
#> 204                         Basa Ugi
#> 205                            ᨅᨔ ᨕᨘᨁᨗ
#> 206                                 
#> 207                                 
#> 208                                 
#> 209                           буряад
#> 210                                 
#> 211                                 
#> 212                                 
#> 213                                 
#> 214                           català
#> 215                                 
#> 216                                 
#> 217                                 
#> 218                                 
#> 219                                 
#> 220                                 
#> 221                                 
#> 222           Chavacano de Zamboanga
#> 223           Chavacano de Zamboanga
#> 224                                 
#> 225                             𑄌𑄋𑄴𑄟𑄳𑄦
#> 226                                 
#> 227           閩東語 / Mìng-dĕ̤ng-ngṳ̄
#> 228                                 
#> 229               閩東語（傳統漢字）
#> 230       Mìng-dĕ̤ng-ngṳ̄ (Bàng-uâ-cê)
#> 231                                 
#> 232                          нохчийн
#> 233                          Cebuano
#> 234                                 
#> 235                                 
#> 236                          Chamoru
#> 237                                 
#> 238                                 
#> 239                                 
#> 240                                 
#> 241                      chinuk wawa
#> 242                    Chahta anumpa
#> 243                                 
#> 244                              ᏣᎳᎩ
#> 245                  Tsetsêhestâhese
#> 246                                 
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
#> 259                            کوردی
#> 260                                 
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
#> 277                            corsu
#> 278                                 
#> 279                     ϯⲙⲉⲧⲣⲉⲙⲛ̀ⲭⲏⲙⲓ
#> 280                                 
#> 281                                 
#> 282                                 
#> 283                         Capiceño
#> 284              莆仙語 / Pó-sing-gṳ̂
#> 285                   莆仙语（简体）
#> 286                   莆仙語（繁體）
#> 287           Pó-sing-gṳ̂ (Báⁿ-uā-ci̍)
#> 288            Nēhiyawēwin / ᓀᐦᐃᔭᐍᐏᐣ
#> 289                                 
#> 290                                 
#> 291                                 
#> 292                                 
#> 293                     qırımtatarca
#> 294          къырымтатарджа (Кирилл)
#> 295             qırımtatarca (Latin)
#> 296                          tatarşa
#> 297                                 
#> 298                                 
#> 299                                 
#> 300                                 
#> 301                                 
#> 302                                 
#> 303                                 
#> 304                          čeština
#> 305                       kaszëbsczi
#> 306                                 
#> 307                                 
#> 308          словѣньскъ / ⰔⰎⰑⰂⰡⰐⰠⰔⰍⰟ
#> 309                                 
#> 310                          чӑвашла
#> 311                          Cymraeg
#> 312                            dansk
#> 313                         dagbanli
#> 314                                 
#> 315                                 
#> 316                                 
#> 317                                 
#> 318                                 
#> 319                                 
#> 320                          Deutsch
#> 321                                 
#> 322         Österreichisches Deutsch
#> 323            Schweizer Hochdeutsch
#> 324               Deutsch (Sie-Form)
#> 325                                 
#> 326                                 
#> 327                          Dagaare
#> 328                                 
#> 329                         Thuɔŋjäŋ
#> 330                           Zazaki
#> 331                                 
#> 332                                 
#> 333                                 
#> 334                      долган тыла
#> 335                                 
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
#> 346                     dolnoserbski
#> 347                                 
#> 348                                 
#> 349                     Kadazandusun
#> 350                                 
#> 351                            डोटेली
#> 352                            Duálá
#> 353                                 
#> 354                                 
#> 355                            ދިވެހިބަސް
#> 356                                 
#> 357                                 
#> 358                             ཇོང་ཁ
#> 359                                 
#> 360                                 
#> 361                           eʋegbe
#> 362                             Efịk
#> 363               emiliàn e rumagnòl
#> 364                                 
#> 365                                 
#> 366                                 
#> 367                         Ελληνικά
#> 368                                 
#> 369                                 
#> 370                                 
#> 371                                 
#> 372               emiliàn e rumagnòl
#> 373                          English
#> 374                                 
#> 375                 Canadian English
#> 376                                 
#> 377                                 
#> 378                  British English
#> 379                                 
#> 380                                 
#> 381                                 
#> 382                                 
#> 383                   Simple English
#> 384                                 
#> 385                                 
#> 386                                 
#> 387                        Esperanto
#> 388                                 
#> 389                                 
#> 390                                 
#> 391                          español
#> 392        español de América Latina
#> 393                                 
#> 394                 español (formal)
#> 395                                 
#> 396                                 
#> 397                                 
#> 398                                 
#> 399                            eesti
#> 400                                 
#> 401                                 
#> 402                                 
#> 403                          euskara
#> 404                                 
#> 405                        estremeñu
#> 406                                 
#> 407                            فارسی
#> 408                                 
#> 409                                 
#> 410                                 
#> 411                                 
#> 412                          mfantse
#> 413                                 
#> 414                                 
#> 415                         Fulfulde
#> 416                            suomi
#> 417                                 
#> 418                        meänkieli
#> 419                                 
#> 420                             võro
#> 421                 Na Vosa Vakaviti
#> 422                                 
#> 423                                 
#> 424                         føroyskt
#> 425                           fɔ̀ngbè
#> 426                                 
#> 427                         français
#> 428                                 
#> 429                                 
#> 430                                 
#> 431                  français cadien
#> 432                                 
#> 433                                 
#> 434                                 
#> 435                          arpetan
#> 436                       Nordfriisk
#> 437                       Oostfräisk
#> 438                                 
#> 439                                 
#> 440                                 
#> 441                           furlan
#> 442                   poor’íŋ belé’ŋ
#> 443                            Frysk
#> 444                          Gaeilge
#> 445                               Ga
#> 446                           Gagauz
#> 447                                 
#> 448                             贛語
#> 449                     赣语（简体）
#> 450                     贛語（繁體）
#> 451                                 
#> 452                                 
#> 453                                 
#> 454                                 
#> 455                                 
#> 456                                 
#> 457                                 
#> 458                                 
#> 459                  kréyòl Gwadloup
#> 460                 kriyòl gwiyannen
#> 461                         Gàidhlig
#> 462                                 
#> 463                                 
#> 464                                 
#> 465                                 
#> 466                                 
#> 467                                 
#> 468                           galego
#> 469                             на̄ни
#> 470                                 
#> 471                            گیلکی
#> 472                                 
#> 473                                 
#> 474                                 
#> 475                          Avañe'ẽ
#> 476                                 
#> 477                                 
#> 478     गोंयची कोंकणी / Gõychi Konknni
#> 479                      गोंयची कोंकणी
#> 480                   Gõychi Konknni
#> 481                                 
#> 482                 Bahasa Hulontalo
#> 483                           𐌲𐌿𐍄𐌹𐍃𐌺
#> 484                  Ghanaian Pidgin
#> 485                                 
#> 486                  Ἀρχαία ἑλληνικὴ
#> 487                                 
#> 488                      Alemannisch
#> 489                                 
#> 490                           ગુજરાતી
#> 491                       wayuunaiki
#> 492                                 
#> 493                         farefare
#> 494                           gungbe
#> 495                                 
#> 496                            Gaelg
#> 497                                 
#> 498                                 
#> 499                            Hausa
#> 500                                 
#> 501                                 
#> 502                                 
#> 503                                 
#> 504                                 
#> 505              客家語 / Hak-kâ-ngî
#> 506                   客家语（简体）
#> 507                   客家語（繁體）
#> 508          Hak-kâ-ngî (Pha̍k-fa-sṳ)
#> 509                                 
#> 510                          Hawaiʻi
#> 511                                 
#> 512                                 
#> 513                                 
#> 514                            עברית
#> 515                                 
#> 516                            हिन्दी
#> 517                                 
#> 518                                 
#> 519                       Fiji Hindi
#> 520                                 
#> 521                       Fiji Hindi
#> 522                          Ilonggo
#> 523                                 
#> 524                                 
#> 525                                 
#> 526                                 
#> 527                          kihunde
#> 528                                 
#> 529                                 
#> 530                                 
#> 531                            ہندکو
#> 532                        Hiri Motu
#> 533                                 
#> 534                               Ho
#> 535                         hrvatski
#> 536                                 
#> 537                          Hunsrik
#> 538                    hornjoserbsce
#> 539                             湘語
#> 540                                 
#> 541                                 
#> 542                   Kreyòl ayisyen
#> 543                                 
#> 544                           magyar
#> 545                  magyar (formal)
#> 546                                 
#> 547                                 
#> 548                          հայերեն
#> 549                   Արեւմտահայերէն
#> 550                       Otsiherero
#> 551                      interlingua
#> 552                        Jaku Iban
#> 553                           ibibio
#> 554                 Bahasa Indonesia
#> 555                      Interlingue
#> 556                                 
#> 557                             Igbo
#> 558                                 
#> 559                            Igala
#> 560                             ꆇꉙ
#> 561                                 
#> 562                        Iñupiatun
#> 563                           ᐃᓄᒃᑎᑐᑦ
#> 564                        inuktitut
#> 565                                 
#> 566                          Ilokano
#> 567                                 
#> 568                                 
#> 569                         гӀалгӀай
#> 570                              Ido
#> 571                                 
#> 572                                 
#> 573                         íslenska
#> 574                                 
#> 575                                 
#> 576                                 
#> 577                                 
#> 578                                 
#> 579                                 
#> 580                  medžuslovjansky
#> 581                  меджусловјанскы
#> 582                  medžuslovjansky
#> 583                         italiano
#> 584               ᐃᓄᒃᑎᑐᑦ / inuktitut
#> 585                                 
#> 586                                 
#> 587                                 
#> 588                           日本語
#> 589                                 
#> 590                                 
#> 591                                 
#> 592                                 
#> 593                                 
#> 594                                 
#> 595                           Patois
#> 596                                 
#> 597                      la .lojban.
#> 598                                 
#> 599                                 
#> 600                                 
#> 601                                 
#> 602                                 
#> 603                                 
#> 604                                 
#> 605                                 
#> 606                             jysk
#> 607                             Jawa
#> 608                               ꦗꦮ
#> 609                          ქართული
#> 610                    Qaraqalpaqsha
#> 611                        Taqbaylit
#> 612                                 
#> 613                                 
#> 614                      Karai-karai
#> 615                              Jju
#> 616                                 
#> 617                                 
#> 618                                 
#> 619                         адыгэбзэ
#> 620                         адыгэбзэ
#> 621                                 
#> 622                                 
#> 623                           Kabɩyɛ
#> 624                             Tyap
#> 625                                 
#> 626                                 
#> 627                     kabuverdianu
#> 628                                 
#> 629                                 
#> 630                                 
#> 631                                 
#> 632                                 
#> 633                            Kongo
#> 634                         Kumoring
#> 635                                 
#> 636                                 
#> 637                                 
#> 638                                 
#> 639                                 
#> 640                                 
#> 641                                 
#> 642                            کھوار
#> 643                           Gĩkũyũ
#> 644                                 
#> 645                        Kırmancki
#> 646                                 
#> 647                         Kwanyama
#> 648                            хакас
#> 649                              ဖၠုံလိက်
#> 650                          қазақша
#> 651                  قازاقشا (تٴوتە)
#> 652                  قازاقشا (جۇنگو)
#> 653                  қазақша (кирил)
#> 654              қазақша (Қазақстан)
#> 655                  qazaqşa (latın)
#> 656                qazaqşa (Türkïya)
#> 657                                 
#> 658                      kalaallisut
#> 659                                 
#> 660                                 
#> 661                                 
#> 662                                 
#> 663                                 
#> 664                         ភាសាខ្មែរ
#> 665                                 
#> 666                                 
#> 667                                 
#> 668                                 
#> 669                                 
#> 670                             ಕನ್ನಡ
#> 671                     Yerwa Kanuri
#> 672                                 
#> 673                                 
#> 674                                 
#> 675                           한국어
#> 676                                 
#> 677                                 
#> 678                                 
#> 679                           조선말
#> 680                                 
#> 681                       перем коми
#> 682                                 
#> 683                                 
#> 684                                 
#> 685                                 
#> 686                                 
#> 687                                 
#> 688                                 
#> 689                           kanuri
#> 690                 къарачай-малкъар
#> 691                             Krio
#> 692                        Kinaray-a
#> 693                           karjal
#> 694                                 
#> 695                                 
#> 696                             کٲشُر
#> 697                             کٲشُر
#> 698                             कॉशुर
#> 699                                 
#> 700                                 
#> 701                       Ripoarisch
#> 702                               စှီၤ
#> 703                                 
#> 704                            kurdî
#> 705                   کوردی (عەرەبی)
#> 706                   kurdî (latînî)
#> 707                          къумукъ
#> 708                           Kʋsaal
#> 709                                 
#> 710                             коми
#> 711                                 
#> 712                         kernowek
#> 713                                 
#> 714                                 
#> 715                                 
#> 716                                 
#> 717                                 
#> 718                         кыргызча
#> 719                                 
#> 720                                 
#> 721                           Latina
#> 722                           Ladino
#> 723                                 
#> 724                                 
#> 725                                 
#> 726                                 
#> 727                                 
#> 728                                 
#> 729                   Lëtzebuergesch
#> 730                            лакку
#> 731                                 
#> 732                                 
#> 733                                 
#> 734                            лезги
#> 735               Lingua Franca Nova
#> 736                          Luganda
#> 737                         Limburgs
#> 738                                 
#> 739                                 
#> 740                           Ligure
#> 741                                 
#> 742                                 
#> 743                         Līvõ kēļ
#> 744                      Lampung Api
#> 745                             لەکی
#> 746                      Lakȟótiyapi
#> 747                            Ladin
#> 748                                 
#> 749                                 
#> 750                                 
#> 751                                 
#> 752                                 
#> 753                          lombard
#> 754                          lingála
#> 755                                 
#> 756                              ລາວ
#> 757                                 
#> 758                                 
#> 759                                 
#> 760                           Silozi
#> 761                      لۊری شومالی
#> 762                                 
#> 763                         lietuvių
#> 764                          latgaļu
#> 765                                 
#> 766                           ciluba
#> 767                                 
#> 768                                 
#> 769                                 
#> 770                                 
#> 771                       Mizo ţawng
#> 772                                 
#> 773                                 
#> 774                      لئری دوٙمینی
#> 775                         latviešu
#> 776                             文言
#> 777                           Lazuri
#> 778                          Madhurâ
#> 779                                 
#> 780                             मगही
#> 781                            मैथिली
#> 782                                 
#> 783                                 
#> 784                                 
#> 785                                 
#> 786                  Basa Banyumasan
#> 787                                 
#> 788                                 
#> 789                                 
#> 790                                 
#> 791                                 
#> 792                          мокшень
#> 793                                 
#> 794                                 
#> 795                                 
#> 796                                 
#> 797                                 
#> 798                                 
#> 799                                 
#> 800                         Malagasy
#> 801                                 
#> 802                                 
#> 803                                 
#> 804                             Ebon
#> 805                                 
#> 806                                 
#> 807                       олык марий
#> 808                            Māori
#> 809                                 
#> 810                                 
#> 811                      Minangkabau
#> 812                                 
#> 813                                 
#> 814                                 
#> 815                                 
#> 816                                 
#> 817                       македонски
#> 818                                 
#> 819                           മലയാളം
#> 820                           монгол
#> 821                                 
#> 822                                 
#> 823                      manju gisun
#> 824                      manju gisun
#> 825                      ᠮᠠᠨᠵᡠ ᡤᡳᠰᡠᠨ
#> 826                         ꯃꯤꯇꯩ ꯂꯣꯟ
#> 827                                 
#> 828                                 
#> 829                                 
#> 830                                 
#> 831                                 
#> 832                           ဘာသာမန်
#> 833                     молдовеняскэ
#> 834                                 
#> 835                                 
#> 836                            moore
#> 837                            मराठी
#> 838                                 
#> 839                             Mara
#> 840                       кырык мары
#> 841                                 
#> 842                                 
#> 843                    Bahasa Melayu
#> 844                       بهاس ملايو
#> 845                                 
#> 846                            Malti
#> 847                                 
#> 848                   Baso Palembang
#> 849                                 
#> 850                                 
#> 851                          Mvskoke
#> 852                                 
#> 853                                 
#> 854                                 
#> 855                                 
#> 856                         Mirandés
#> 857                                 
#> 858                                 
#> 859                                 
#> 860                                 
#> 861                        မြန်မာဘာသာ
#> 862                                 
#> 863                                 
#> 864                           эрзянь
#> 865                          مازِرونی
#> 866                   Dorerin Naoero
#> 867                          Nāhuatl
#> 868                                 
#> 869              閩南語 / Bân-lâm-gí
#> 870                                 
#> 871                                 
#> 872               閩南語（傳統漢字）
#> 873           Bân-lâm-gí (Pe̍h-ōe-jī)
#> 874              Bân-lâm-gí (Tâi-lô)
#> 875                       Napulitano
#> 876                                 
#> 877                     norsk bokmål
#> 878                                 
#> 879                     Plattdüütsch
#> 880                     Nedersaksies
#> 881                            नेपाली
#> 882                        नेपाल भाषा
#> 883                        Oshiwambo
#> 884                                 
#> 885                          Li Niha
#> 886                                 
#> 887                              కొలామి
#> 888                             Niuē
#> 889                                 
#> 890                       Nederlands
#> 891                                 
#> 892                                 
#> 893                                 
#> 894           Nederlands (informeel)
#> 895                                 
#> 896                                 
#> 897                                 
#> 898                                 
#> 899                                 
#> 900                                 
#> 901                            nawdm
#> 902                    norsk nynorsk
#> 903                                 
#> 904                                 
#> 905                                 
#> 906                            norsk
#> 907                            ᨣᩤᩴᨾᩮᩬᩥᨦ
#> 908                                 
#> 909                          ногайша
#> 910                                 
#> 911                                 
#> 912                           Novial
#> 913                              ߒߞߏ
#> 914              isiNdebele seSewula
#> 915                                 
#> 916                                 
#> 917                        Nouormand
#> 918                                 
#> 919                                 
#> 920                 Sesotho sa Leboa
#> 921                                 
#> 922                                 
#> 923                             Nupe
#> 924                                 
#> 925                      Diné bizaad
#> 926                                 
#> 927                                 
#> 928                        Chi-Chewa
#> 929                                 
#> 930                       runyankore
#> 931                         Orunyoro
#> 932                           Nyunga
#> 933                                 
#> 934                                 
#> 935                          occitan
#> 936                                 
#> 937                                 
#> 938                                 
#> 939                                 
#> 940                      Ojibwemowin
#> 941                                 
#> 942                                 
#> 943                                 
#> 944                                 
#> 945                                 
#> 946                                 
#> 947                                 
#> 948                    livvinkarjala
#> 949                           Oromoo
#> 950                                 
#> 951                                 
#> 952                              ଓଡ଼ିଆ
#> 953                             ирон
#> 954                                 
#> 955                                 
#> 956                                 
#> 957                                 
#> 958                                 
#> 959                                 
#> 960                                 
#> 961                                 
#> 962                                 
#> 963                                 
#> 964                            ਪੰਜਾਬੀ
#> 965                                 
#> 966                                 
#> 967                       Pangasinan
#> 968                                 
#> 969                                 
#> 970                                 
#> 971                                 
#> 972                      Kapampangan
#> 973                                 
#> 974                       Papiamentu
#> 975               Papiamento (Aruba)
#> 976                                 
#> 977                                 
#> 978                                 
#> 979                           Picard
#> 980                                 
#> 981                                 
#> 982                            Naijá
#> 983                          Deitsch
#> 984                     Plautdietsch
#> 985                                 
#> 986                         Pälzisch
#> 987                                 
#> 988                                 
#> 989                                 
#> 990                                 
#> 991                                 
#> 992                                 
#> 993                                 
#> 994                                 
#> 995                                 
#> 996                                 
#> 997                                 
#> 998                             पालि
#> 999                                 
#> 1000                Norfuk / Pitkern
#> 1001                                
#> 1002                                
#> 1003                                
#> 1004                                
#> 1005                                
#> 1006                          polski
#> 1007                                
#> 1008                                
#> 1009                                
#> 1010                      Piemontèis
#> 1011                          پنجابی
#> 1012                        Ποντιακά
#> 1013                                
#> 1014                                
#> 1015                           Nawat
#> 1016                                
#> 1017                                
#> 1018                                
#> 1019                                
#> 1020                       prūsiskan
#> 1021                                
#> 1022                                
#> 1023                            پښتو
#> 1024                                
#> 1025                                
#> 1026                                
#> 1027                                
#> 1028                                
#> 1029                                
#> 1030                                
#> 1031                                
#> 1032                                
#> 1033                       português
#> 1034                                
#> 1035             português do Brasil
#> 1036                                
#> 1037                                
#> 1038                      pinayuanan
#> 1039                                
#> 1040                                
#> 1041                                
#> 1042                       Runa Simi
#> 1043                                
#> 1044                      Runa shimi
#> 1045                                
#> 1046                                
#> 1047                                
#> 1048                                
#> 1049                                
#> 1050                                
#> 1051                                
#> 1052                                
#> 1053                                
#> 1054                                
#> 1055                                
#> 1056                        Rumagnôl
#> 1057                                
#> 1058                                
#> 1059                                
#> 1060                         Tarifit
#> 1061                                
#> 1062                             ရခိုင်
#> 1063                                
#> 1064                       rumantsch
#> 1065                                
#> 1066                                
#> 1067                                
#> 1068                                
#> 1069                                
#> 1070                                
#> 1071                     romaňi čhib
#> 1072                                
#> 1073                                
#> 1074                                
#> 1075                                
#> 1076                                
#> 1077                                
#> 1078                                
#> 1079                     romani čhib
#> 1080                        ikirundi
#> 1081                                
#> 1082                          română
#> 1083                                
#> 1084                                
#> 1085                     armãneashti
#> 1086                       tarandíne
#> 1087                                
#> 1088                                
#> 1089                           руски
#> 1090                                
#> 1091                         русский
#> 1092                                
#> 1093                      русиньскый
#> 1094                                
#> 1095                                
#> 1096                     armãneashti
#> 1097                        Vlăheşte
#> 1098                        Влахесте
#> 1099                        Vlăheşte
#> 1100                      мыхаӀбишды
#> 1101                    Ikinyarwanda
#> 1102                                
#> 1103                                
#> 1104                                
#> 1105                                
#> 1106                    うちなーぐち
#> 1107                                
#> 1108                           संस्कृतम्
#> 1109                                
#> 1110                                
#> 1111                       саха тыла
#> 1112                                
#> 1113                                
#> 1114                                
#> 1115                                
#> 1116                           Sasak
#> 1117                         ᱥᱟᱱᱛᱟᱲᱤ
#> 1118                                
#> 1119                                
#> 1120                                
#> 1121                                
#> 1122                                
#> 1123                                
#> 1124                           sardu
#> 1125                                
#> 1126                                
#> 1127                       sicilianu
#> 1128                           Scots
#> 1129                                
#> 1130                            سنڌي
#> 1131                                
#> 1132                                
#> 1133                                
#> 1134                                
#> 1135                       Sassaresu
#> 1136                     کوردی خوارگ
#> 1137                                
#> 1138                                
#> 1139                                
#> 1140                 davvisámegiella
#> 1141  davvisámegiella (Suoma bealde)
#> 1142 davvisámegiella (Norgga bealde)
#> 1143  davvisámegiella (Ruoŧa bealde)
#> 1144                                
#> 1145                                
#> 1146                                
#> 1147                     Cmique Itom
#> 1148                                
#> 1149                                
#> 1150                                
#> 1151                 Koyraboro Senni
#> 1152                                
#> 1153                           Sängö
#> 1154                                
#> 1155                                
#> 1156                                
#> 1157                                
#> 1158                                
#> 1159                                
#> 1160                      žemaitėška
#> 1161                                
#> 1162                                
#> 1163 srpskohrvatski / српскохрватски
#> 1164       српскохрватски (ћирилица)
#> 1165       srpskohrvatski (latinica)
#> 1166                                
#> 1167                         Taclḥit
#> 1168                         Taclḥit
#> 1169                         ⵜⴰⵛⵍⵃⵉⵜ
#> 1170                              တႆး
#> 1171                                
#> 1172                         tacawit
#> 1173                                
#> 1174                         tacawit
#> 1175                                
#> 1176                            සිංහල
#> 1177                                
#> 1178                                
#> 1179                  Simple English
#> 1180                                
#> 1181                                
#> 1182                 кӣллт са̄мь кӣлл
#> 1183                 bidumsámegiella
#> 1184                                
#> 1185                                
#> 1186                                
#> 1187                                
#> 1188                                
#> 1189                                
#> 1190                      slovenčina
#> 1191                         سرائیکی
#> 1192                         سرائیکی
#> 1193                     slovenščina
#> 1194                                
#> 1195                                
#> 1196                        Schläsch
#> 1197                                
#> 1198                                
#> 1199                    Gagana Samoa
#> 1200                   åarjelsaemien
#> 1201                                
#> 1202                                
#> 1203                     anarâškielâ
#> 1204                nuõrttsääʹmǩiõll
#> 1205                        chiShona
#> 1206                                
#> 1207                                
#> 1208                      Soomaaliga
#> 1209                                
#> 1210                                
#> 1211                                
#> 1212                           shqip
#> 1213                 српски / srpski
#> 1214               српски (ћирилица)
#> 1215               српски (ћирилица)
#> 1216               srpski (latinica)
#> 1217               srpski (latinica)
#> 1218                                
#> 1219                                
#> 1220                                
#> 1221                                
#> 1222                                
#> 1223                     Sranantongo
#> 1224               sardu campidanesu
#> 1225                                
#> 1226                                
#> 1227                         SiSwati
#> 1228                                
#> 1229                                
#> 1230                                
#> 1231                                
#> 1232                         Sesotho
#> 1233                                
#> 1234                       Seeltersk
#> 1235                                
#> 1236                      себертатар
#> 1237                           Sunda
#> 1238                                
#> 1239                                
#> 1240                                
#> 1241                                
#> 1242                                
#> 1243                                
#> 1244                         svenska
#> 1245                                
#> 1246                                
#> 1247                       Kiswahili
#> 1248                                
#> 1249                                
#> 1250                                
#> 1251                                
#> 1252                                
#> 1253                                
#> 1254                                
#> 1255                                
#> 1256                           ꠍꠤꠟꠐꠤ
#> 1257                                
#> 1258                                
#> 1259                                
#> 1260                         ślůnski
#> 1261                        Sakizaya
#> 1262                            தமிழ்
#> 1263                                
#> 1264                                
#> 1265                           Tayal
#> 1266                                
#> 1267                                
#> 1268                            ತುಳು
#> 1269                    ᥖᥭᥰ ᥖᥬᥲ ᥑᥨᥒᥰ
#> 1270                           తెలుగు
#> 1271                                
#> 1272                                
#> 1273                                
#> 1274                           tetun
#> 1275                          тоҷикӣ
#> 1276                          тоҷикӣ
#> 1277                          tojikī
#> 1278                                
#> 1279                             ไทย
#> 1280                                
#> 1281                                
#> 1282                                
#> 1283                            ትግርኛ
#> 1284                             ትግሬ
#> 1285                                
#> 1286                                
#> 1287                                
#> 1288                       Türkmençe
#> 1289                                
#> 1290                                
#> 1291                         Tagalog
#> 1292                                
#> 1293                                
#> 1294                                
#> 1295                                
#> 1296                                
#> 1297                          tolışi
#> 1298                          толыши
#> 1299                                
#> 1300                                
#> 1301                        Setswana
#> 1302                                
#> 1303                  lea faka-Tonga
#> 1304                                
#> 1305                                
#> 1306                       toki pona
#> 1307                       Tok Pisin
#> 1308                          Türkçe
#> 1309                                
#> 1310                          Ṫuroyo
#> 1311                          Seediq
#> 1312                                
#> 1313                        Xitsonga
#> 1314                                
#> 1315                                
#> 1316                                
#> 1317                                
#> 1318                                
#> 1319               татарча / tatarça
#> 1320                         татарча
#> 1321                         tatarça
#> 1322                        Orutooro
#> 1323                                
#> 1324                                
#> 1325                                
#> 1326                      chiTumbuka
#> 1327                                
#> 1328                                
#> 1329                                
#> 1330                                
#> 1331                             Twi
#> 1332                                
#> 1333                                
#> 1334                                
#> 1335                                
#> 1336                                
#> 1337                                
#> 1338                                
#> 1339                      reo tahiti
#> 1340                        тыва дыл
#> 1341                                
#> 1342                        ⵜⴰⵎⴰⵣⵉⵖⵜ
#> 1343                                
#> 1344                          удмурт
#> 1345            ئۇيغۇرچە / Uyghurche
#> 1346                        ئۇيغۇرچە
#> 1347                                
#> 1348                       Uyghurche
#> 1349                                
#> 1350                      українська
#> 1351                                
#> 1352                                
#> 1353                                
#> 1354                                
#> 1355                                
#> 1356                                
#> 1357                                
#> 1358                                
#> 1359                                
#> 1360                            اردو
#> 1361                                
#> 1362                                
#> 1363                                
#> 1364             oʻzbekcha / ўзбекча
#> 1365                         ўзбекча
#> 1366                       oʻzbekcha
#> 1367                                
#> 1368                                
#> 1369                       Tshivenda
#> 1370                          vèneto
#> 1371                     vepsän kel’
#> 1372                                
#> 1373                      Tiếng Việt
#> 1374                                
#> 1375                      West-Vlams
#> 1376                                
#> 1377                                
#> 1378                                
#> 1379                   Mainfränkisch
#> 1380                        emakhuwa
#> 1381                         Volapük
#> 1382                           Vaďďa
#> 1383                            võro
#> 1384                                
#> 1385                                
#> 1386                           walon
#> 1387                                
#> 1388                                
#> 1389                        wolaytta
#> 1390                         Winaray
#> 1391                                
#> 1392                                
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
#> 1403                       Fakaʻuvea
#> 1404                           waale
#> 1405                           Wolof
#> 1406                                
#> 1407                                
#> 1408                            吴语
#> 1409                    吴语（简体）
#> 1410                    吳語（正體）
#> 1411                                
#> 1412                                
#> 1413                          хальмг
#> 1414                                
#> 1415                        isiXhosa
#> 1416                       მარგალური
#> 1417                                
#> 1418                                
#> 1419                                
#> 1420                                
#> 1421                                
#> 1422                                
#> 1423                                
#> 1424                                
#> 1425                                
#> 1426                                
#> 1427                        saisiyat
#> 1428                                
#> 1429                                
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
#> 1442                           ייִדיש
#> 1443                                
#> 1444                          Yorùbá
#> 1445                                
#> 1446                                
#> 1447                                
#> 1448                                
#> 1449                                
#> 1450                                
#> 1451                        Nhẽẽgatú
#> 1452                     maaya t’aan
#> 1453                            粵語
#> 1454                    粵语（简体）
#> 1455                    粵語（繁體）
#> 1456                       Vahcuengh
#> 1457                                
#> 1458                                
#> 1459                                
#> 1460                          Zeêuws
#> 1461                                
#> 1462               ⵜⴰⵎⴰⵣⵉⵖⵜ ⵜⴰⵏⴰⵡⴰⵢⵜ
#> 1463               tamaziɣt tanawayt
#> 1464                            中文
#> 1465                            文言
#> 1466                中文（中国大陆）
#> 1467                    中文（简体）
#> 1468                    中文（繁體）
#> 1469                    中文（香港）
#> 1470             閩南語 / Bân-lâm-gí
#> 1471                    中文（澳門）
#> 1472                中文（马来西亚）
#> 1473                  中文（新加坡）
#> 1474                    中文（臺灣）
#> 1475                            粵語
#> 1476                                
#> 1477                                
#> 1478                                
#> 1479                         isiZulu
#> 1480                                
#> 1481                                
#> 1482                                
# }
```
