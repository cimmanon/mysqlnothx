-- MySQL dump 10.13  Distrib 5.1.73, for unknown-openbsd5.6 (x86_64)
--
-- Host: localhost    Database: foo
-- ------------------------------------------------------
-- Server version	5.1.73-log

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `blob_test`
--

DROP TABLE IF EXISTS `blob_test`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `blob_test` (
  `blob_test` blob
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `blob_test`
--

LOCK TABLES `blob_test` WRITE;
/*!40000 ALTER TABLE `blob_test` DISABLE KEYS */;
INSERT INTO `blob_test` VALUES ('This is a test text file.\n'),('‰PNG\r\n\Z\n\0\0\0\rIHDR\0\0\0\0\0\0\0\0\0;mGú\0\0\n0iCCPICC profile\0\0H‰–wTT×‡Ï½wz¡Í0)Cï½\r ½7©ÒDa˜`(34±!¢EDA‚\"Œ†\"±\"Š…€`Á  Ä`QQy3²Vtåå½——ßg}kŸ½÷=gï}Öº\0¼ý¹¼tX\n€4ž€âåJŒŠ¦cûð\0Ì\0`²23B=Ã€H>nôL‘ø\"€7wÄ+\07¼ƒètðÿIš•ÁˆÒ‰Ø‚ÍÉd‰¸PÄ©Ù‚±}FÄÔø1Ã(1óE±¼˜Ùð³Ï\";‹™Æc‹X|æv\Z[Ì=\"Þš%äˆñqQ—“-â[\"ÖL¦qEüV›Æaf€\"‰í+IÄ¦\"&ñÃBÜD¼\0)ñ+ŽÿŠœøRné¹|nb’€®ËÒ£›ÙÚ2èÞœìTŽ@`Äd¥0ùlº[zZ“—Àâ?KF\\[º¨ÈÖf¶ÖÖFæÆf_ê¿nþM‰{»H¯‚?÷¢õ}±ý•_z=\0ŒYQmv|±Åï c3\0ò÷¿Ø4 )ê[ûÀW÷¡‰ç%I È°31ÉÎÎ6ærXÆâ‚þ¡ÿéð7ôÕ÷ŒÅéþ(Ý“À¦\nèâº±ÒSÓ…|zf“Å¡ýyˆÿqà_ŸÃ0„“Àásx¢ˆpÑ”qy‰¢vóØ\\7GçòþSÿaØŸ´8×\"Q\Z>j¬1\Z ä×>€¢s@´ýÑ7|8¿¼Õ‰Å¹ÿ,èß³Âeâ%“›ø9Î-$ŒÎò³÷ÄÏ H*P\0*@è#`l€=pÀ‚0VHi€²A>ØŠ@	ØvƒjP\Z@h\'@8\r.€Ëà:¸nƒ`Œƒç`¼óa!2D UH2€Ì!äy@þPÅA‰BùÐ&¨*‡ª¡:¨	ú:]€®BƒÐ=hš‚~‡ÞÃL‚©°2¬\r›ÀØöƒÃà•p\"¼\ZÎƒáíp\\ƒÛáðuø6<?‡g€\Z¢†!Ä\r	D¢‘„¬CŠ‘J¤iAº^ä&2‚L#ïPEG¡ìQÞ¨å(j5jªU:‚jGõ n¢FQ3¨Oh2Z	m€¶Cû #Ñ‰èltºÝˆnC_BßF£ß`0\ZFcƒñÆDa’1k0¥˜ý˜VÌyÌ f3‹Åb°Xl –‰`‹°{±Ç°ç°CØqì[§Š3Çyâ¢q<\\®ww7„›ÀÍã¥ðZx;| žÏÅ—áð]øü8~ž MÐ!8ÂÉ„„*Báá!á‘HT\'Úƒ‰\\âbñ8ñ\nq”øŽ$CÒ\'¹‘bHBÒvÒaÒyÒ=Ò+2™¬Mv&G“äíä&òEòcò[	Š„±„[b½DD»ÄÄI¼¤–¤‹ä*É<ÉJÉ“’’ÓRx)m)7)¦Ô:©\Z©SRÃR³Òi3é@é4éRé£ÒW¥\'e°2Ú22l™B™C2eÆ(EƒâFaQ6Q\Z(—(ãTU‡êCM¦–P¿£öSgded-eÃesdkdÏÈŽÐš6Í‡–J+£ Ý¡½—S–s‘ãÈm“k‘’›“_\"ï,Ï‘/–o•¿-ÿ^®à¡¢°S¡Cá‘\"JQ_1X1[ñ€â%Åé%Ô%öKXKŠ—œXr_	VÒW\nQZ£tH©OiVYEÙK9Cy¯òEåišŠ³J²J…ÊY•)UŠª£*WµBõœê3º,Ý…žJ¯¢÷ÐgÔ”Ô¼Õ„jujýjóê:êËÕÔ[Õi4\Z	\Z\ZÝ\Z3šªššùšÍš÷µðZ­$­=Z½ZsÚ:ÚÚ[´;´\'uäu|tòtšuê’utWëÖëÞÒÃè1ôRôöëÝÐ‡õ­ô“ôkô`k®Á~ƒAC´¡­!Ï°ÞpØˆdäb”eÔl4jL3ö7.0î0~a¢im²Ó¤×ä“©•iªiƒé33_³³.³ßÍõÍYæ5æ·,Èžë-:-^Z\ZXr,XÞµ¢XXm±ê¶úhmcÍ·n±ž²Ñ´‰³Ùg3Ì 2‚¥Œ+¶h[WÛõ¶§mßÙYÛ	ìNØýfodŸbÔ~r©ÎRÎÒ†¥cêL‡:‡GºcœãAÇ\'5\'¦S½Óg\rg¶s£ó„‹žK²Ë1—®¦®|×6×97;·µnçÝw/÷b÷~åÕ=Õ==›=g¼¬¼Öx÷F{ûyïôöQöaù4ùÌøÚø®õíñ#ù…úUû=ñ×÷çûwÀ¾».ÓZÆ[Ö}w>\nÒ	Zôc0&8(¸&øiˆYH~Ho(%46ôhè›0×°²°Ëu——w‡K†Ç„7…ÏE¸G”GŒDšD®¼¥ÅêŒÆF‡G7FÏ®ðX±{ÅxŒULQÌ•:+sV^]¥¸*uÕ™XÉXfìÉ8t\\DÜÑ¸Ì@f=s6Þ\'~_üËµ‡õœíÌ®`Oq8åœ‰‡„ò„ÉD‡Ä]‰SINI•IÓ\\7n5÷e²wrmò\\J`Êá”…ÔˆÔÖ4\\Z\\Ú)ž/…×“®’ž“>˜aQ”1²ÚnõîÕ3|?~c&”¹2³S@ýLõ	u…›…£YŽY5Yo³Ã³OæHçðrúrõs·åNäyæ}»µ†µ¦;_-cþèZ—µuë uñëº×k¬/\\?¾ÁkÃ‘„)*0-(/x½)bSW¡rá†Â±Í^››‹$ŠøEÃ[ì·ÔnEmåníßf±mï¶OÅìâk%¦%•%JY¥×¾1û¦ê›…í	ÛûË¬ËìÀìàí¸³Óiç‘réò¼ò±]»Ú+èÅ¯wÇî¾ZiYY»‡°G¸g¤Ê¿ªs¯æÞ{?T\'Uß®q­iÝ§´oÛ¾¹ýìýCœ´Ô*×–Ô¾?È=x·Î«®½^»¾òæPÖ¡§\rá\r½ß2¾mjTl,iüx˜wxäHÈ‘ž&›¦¦£JGËšáfaóÔ±˜c7¾sÿ®³Å¨¥®•ÖZrö}Ü÷wNøè>É8ÙòƒÖûÚ(mÅíP{nûLGRÇHgTçà)ßSÝ]ö]m?\Zÿxø´Úéš3²gÊÎÎž]8—wnö|Æùé‰Æºc»\\Œ¼x«\'¸§ÿ’ß¥+—=/_ìué=wÅáÊé«vWO]c\\ë¸n}½½Ïª¯í\'«ŸÚú­ûÛl:oØÞè\Z\\:xvÈièÂM÷›—oùÜº~{ÙíÁ;ËïÜŽ¹Ë¾;y/õÞËûY÷çlxˆ~XüHêQåc¥Çõ?ëýÜ:b=rfÔ}´ïIè“c¬±ç¿dþòa¼ð)ùiå„êDÓ¤ùäé)Ï©ÏV<žñ|~ºèWé_÷½Ð}ñÃoÎ¿õÍDÎŒ¿ä¿\\ø½ô•Â«Ã¯-_wÏÍ>~“öf~®ø­ÂÛ#ïïzßG¼Ÿ˜Ïþ€ýPõQïc×\'¿OÒþ˜óü7E;\0\0\0	pHYs\0\0\0\0ÒÝ~ü\0\0IDAT8e”Ïk]EÇ¿çœ¹3ó~Ä¾&ZI\nÑÚ(.lA¥bv!XãJ»ÑÚ‚ÒbÀ7EëNÿ\0‹+q£´Å.Ä¶è¦m*X­LmðjÉ3ï½{ß»?fŽ‹ÜJL¿ðe¾ß™9CÎÃV)€31Æ|0p{:…Õ,û-v»ã–1RÆÛÔÚfÈ^\"<EüÐ§¯½þ]úÕ×úÅ[Ë?<hí>\0 \"ÙÎÀÑî‰!Ââý¿|p\"Õ4ÕúÚµ iª¿r²¿¯Óy\0‘0€ÛFÒØDÌØÌ~ôØu½rEó3gªêìÙ˜Ÿ>]éÊeýéÄ‡ÌóLOÄ––D\0ÊL\Z#Þdÿg‡¼z`tãFí³ÌÈpHœeœß\\«çî›ïíÎòG?¿zõ$‹¨ª\0¤µI•ˆ‡‰ß^~né‚©6ij¸ß¯¯#®¯#ôû¼‘¦õB¯÷ÀêÅKÕå¢øÖ2‹j<WŠ°ÇÈÃ/9ÿî?·n¡»²\"±®ˆ MÒÀ˜Yò¢À‹Î¿s~2þr-†ˆM\00†âYç›¢èÜ¼t1Ü==-mU8à ¿ÖÖÃÖíÎñFÃç‰\0Ö=ý¦sßØÑ(Niä^’ Ã»RÕ56BÀˆ9–í.\\–K¿–Å)ÓfÆãÎ/Ë¤Qk\0UUaÀ6WT\0Æ\02\"lÄ¨¨\n<æüòõª8ef»¸“°”V%Z€„&ºk Ô¼âêv%UŒ\0™TvZÿÌýÎ4óÎ¿<	µ)4†6 €¬˜-Iê¦Òdól«‚Ì;ÿŠ™kµåÙ%×1¢`H &	\0„R¡Ø\\¹&`ÆùCfvÇôLi,IQ×Š¨ˆª¨ ˆ\n\0J\0‰4n„^Ýv×t§îºGy>ÚíZ§Lâ½$ä\'	[I8ÃÆ6Æ²1	\'Æ²M,{ë¹xf=ÿwú-..¢çÜ®¹VçÉ‰ÝÛ¹×‹L\'Ìm!2\nÕ ZÕPÄ8ÌCøsXW«é$¿°–e?“ªJSºeºïø3\Zéûü/×ôbL«³x)\0\0\0\0IEND®B`‚');
/*!40000 ALTER TABLE `blob_test` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2017-04-30 14:54:37
