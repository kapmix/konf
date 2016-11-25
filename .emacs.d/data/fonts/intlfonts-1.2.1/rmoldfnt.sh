#!/bin/sh
if [ -z "$1" ] ; then
  echo Usage: $0 DIRECTORY
  exit 1
fi

Asian="etl16-viscii etl24-viscii mule-indian-1col-24\
	mule-indian-24 mule-iscii-24 mule-lao-14\
	mule-lao-16 mule-lao-24 mule-tibmdx-16\
	mule-tibmdx-1col-16 mule-tibmdx-1col-24\
	mule-tibmdx-24 thai-14 thai-16 thai-24"

Chinese="cns-1-16 cns-1-24 cns-2-16 cns-2-24 cns-3-24\
	cns-4-24 cns-5-24 cns-6-24 cns-7-24 etl14-sisheng\
	etl16-sisheng etl24-sisheng guobiao16 taipei16 taipei24"

ChineseBIG="cc40s cc48s cns-1-40 cns-2-40 cns-3-40\
	cns-4-40 cns-5-40 cns-6-40 cns-7-40"

ChineseX="gb16fs gb16st gb24st"

Ethiopic="ethiomx16f-uni ethiomx24f-uni"

European="a18 etl14-cyrillic etl14-greek etl14-koi8\
	etl14-latin1 etl14-latin2 etl14-latin3 etl14-latin4\
	etl14-latin5 etl16-cyrillic etl16-greek etl16-koi8\
	etl16-latin1 etl16-latin2 etl16-latin3 etl16-latin4\
	etl16-latin5 etl16b-latin1 etl16bi-latin1 etl16i-latin1\
	etl24-cyrillic etl24-greek etl24-koi8 etl24-latin1\
	etl24-latin2 etl24-latin3 etl24-latin4 etl24-latin5"

EuropeanBIG="etl40-latin1"

Japanese="a18rk jiskan16-1978 jisksp16 jisksp24 k16-1990 kanji18"

JapaneseBIG="jisksp40 kanji32 kanji48"

JapaneseX="12x24rk 8x16rk jiskan16 jiskan24 k14"

KoreanX="hanglg16 hanglm16 hanglm24"

Misc="etl14-hebrew etl14-ipa etl16-arabic0 etl16-arabic1 etl16-arabic2\
	etl16-hebrew etl16-ipa etl24-arabic0 etl24-arabic1 etl24-arabic2\
	etl24-hebrew etl24-ipa"

cd $1
for f in ${Asian} ${Chinese} ${ChineseBIG} ${ChineseX} ${Ethiopic}\
	${European} ${EuropeanBIG} ${Japanese} ${JapaneseBIG} ${JapaneseX}\
	${KoreanX} ${Misc} ; do
  rm -f $f.*
done
