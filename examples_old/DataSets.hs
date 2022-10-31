module DataSets where

n_counties :: Int
n_counties = 85

logRadon :: [Double]
logRadon = [0.8329091229351041, 0.8329091229351041, 1.0986122886681098, 0.0953101798043249, 1.1631508098056809, 0.9555114450274363, 0.4700036292457356, 0.0953101798043249, -0.2231435513142098, 0.262364264467491, 0.262364264467491, 0.336472236621213, 0.4054651081081644, -0.6931471805599453, 0.1823215567939548, 1.5260563034950492, 0.336472236621213, 0.7884573603642703, 1.791759469228055, 1.2237754316221157, 0.6418538861723948, 1.7047480922384253, 1.856297990365626, 0.6931471805599453, 1.9021075263969205, 1.1631508098056809, 1.9315214116032136, 1.9600947840472696, 2.0541237336955462, 1.667706820558076, 1.5260563034950492, 1.5040773967762742, 1.0647107369924282, 2.1041341542702074, 0.5306282510621705, 1.4586150226995167, 1.7047480922384253, 1.410986973710262, 0.8754687373538999, 1.0986122886681098, 0.4054651081081644, 1.2237754316221157, 1.0986122886681098, 0.6418538861723948, -1.203972804325936, 0.9162907318741552, 0.1823215567939548, 0.8329091229351041, -0.3566749439387324, 0.5877866649021191, 1.0986122886681098, 0.8329091229351041, 0.5877866649021191, 0.4054651081081644, 0.6931471805599453, 0.6418538861723948, 0.262364264467491, 1.4816045409242151, 1.5260563034950492, 1.856297990365626, 1.5475625087160128, 1.7578579175523736, 0.8329091229351041, -0.6931471805599453, 1.5475625087160128, 1.5040773967762742, 1.9021075263969205, 1.0296194171811583, 1.0986122886681098, 1.0986122886681098, 1.9878743481543453, 1.62924053973028, 0.9932517730102834, 1.62924053973028, 2.5726122302071057, 1.9878743481543453, 1.9315214116032136, 2.5572273113676265, 1.7749523509116736, 2.26176309847379, 1.8082887711792652, 1.3609765531356006, 2.667228206581955, 0.6418538861723948, 1.9459101490553128, 1.5686159179138452, 2.26176309847379, 0.9555114450274363, 1.916922612182061, 1.410986973710262, 2.322387720290225, 0.8329091229351041, 0.6418538861723948, 1.252762968495368, 1.7404661748405044, 1.4816045409242151, 1.3862943611198906, 0.336472236621213, 1.4586150226995167, -0.1053605156578262, 0.7419373447293773, 0.5306282510621705, 2.5649493574615367, 2.694627180770069, 1.5686159179138452, 2.272125885509337, -2.302585092994045, 1.3350010667323402, 2.0149030205422647, 0.6931471805599453, 1.6863989535702286, 1.410986973710262, 2.0541237336955462, 0.4054651081081644, 2.312535423847214, 2.2512917986064958, -0.1053605156578262, 1.5040773967762742, 1.62924053973028, 0.7884573603642703, 0.5877866649021191, 2.1041341542702074, 0.0, 2.5649493574615367, 0.9932517730102834, 1.2809338454620642, 3.284663565406204, 0.4700036292457356, 2.5726122302071057, 2.186051276738094, 2.975529566236472, 0.9555114450274363, 2.2082744135228043, 2.580216829592325, 1.308332819650179, 1.9459101490553128, 1.5892352051165808, 1.252762968495368, 0.0, 1.252762968495368, 1.0296194171811583, 0.4054651081081644, 1.9315214116032136, 2.4159137783010487, -2.302585092994045, 0.9555114450274363, 0.6418538861723948, 0.5306282510621705, 0.0953101798043249, 0.0, 1.0986122886681098, 1.5040773967762742, 0.4700036292457356, 1.4350845252893225, 0.9555114450274363, 1.916922612182061, 1.4816045409242151, 1.7227665977411035, 1.308332819650179, 1.0647107369924282, 2.6878474937846906, 1.916922612182061, 2.091864061678393, 0.9932517730102834, 1.0647107369924282, 1.5040773967762742, 0.5877866649021191, 0.7419373447293773, 0.7419373447293773, 0.4700036292457356, 2.272125885509337, 2.1041341542702074, 1.2809338454620642, -0.1053605156578262, 1.6486586255873816, 1.1939224684724346, 2.388762789235098, 2.116255514802552, 1.856297990365626, 1.5892352051165808, 1.8082887711792652, 0.1823215567939548, 2.1747517214841605, 2.186051276738094, 1.9315214116032136, 0.8754687373538999, 0.5306282510621705, 1.0647107369924282, 1.88706964903238, 0.5877866649021191, 1.5475625087160128, 1.2237754316221157, 1.5040773967762742, 3.05870707271538, 2.2192034840549946, 0.0, 1.6094379124341005, 1.62924053973028, 0.1823215567939548, 2.0412203288596382, 1.7047480922384253, 1.308332819650179, 1.6094379124341005, 1.5686159179138452, 0.4054651081081644, 1.252762968495368, 1.4586150226995167, 0.9555114450274363, 0.4054651081081644, 0.4054651081081644, 0.6931471805599453, 1.5892352051165808, 0.4054651081081644, 1.3609765531356006, 2.186051276738094, 1.4816045409242151, 1.5040773967762742, 1.5260563034950492, 0.8329091229351041, -0.5108256237659907, 1.7749523509116736, 1.7047480922384253, 1.9878743481543453, 1.7578579175523736, 2.0149030205422647, 1.5892352051165808, 1.9315214116032136, 1.8718021769015916, 1.3350010667323402, 1.7227665977411035, 2.066862759472976, 1.5040773967762742, 1.0296194171811583, 1.252762968495368, 1.4586150226995167, 0.8754687373538999, 0.336472236621213, 1.667706820558076, -1.6094379124341005, 0.9555114450274363, 1.1939224684724346, 1.1939224684724346, 2.272125885509337, 1.4586150226995167, 2.2082744135228043, 1.856297990365626, 3.487375077903208, 2.587764035227708, 0.8329091229351041, 1.7404661748405044, 2.667228206581955, 1.9459101490553128, 2.0412203288596382, 2.2925347571405443, 0.9932517730102834, 3.775057150354989, 1.6094379124341005, 1.6094379124341005, 1.2809338454620642, 1.5892352051165808, 1.7404661748405044, 1.2809338454620642, 1.3862943611198906, 1.916922612182061, 2.079441541679836, 1.2237754316221157, 0.7884573603642703, 0.5306282510621705, 1.410986973710262, 0.6418538861723948, 0.9555114450274363, 2.424802725718295, 0.9932517730102834, 1.3862943611198906, 2.0149030205422647, 0.336472236621213, 0.0, -0.6931471805599453, 0.9555114450274363, 1.8082887711792652, 0.7419373447293773, 1.7047480922384253, 1.1314021114911006, 1.0986122886681098, 1.7227665977411035, 1.4350845252893225, 1.3862943611198906, 2.70805020110221, 1.9878743481543453, 0.8754687373538999, 1.0647107369924282, 1.5040773967762742, 0.4700036292457356, 2.163323025660538, 1.7404661748405044, 2.163323025660538, 1.3609765531356006, 0.6418538861723948, 0.6931471805599453, 1.7227665977411035, 0.9555114450274363, -0.1053605156578262, 0.7884573603642703, 1.0647107369924282, 1.3862943611198906, 1.4816045409242151, 1.5686159179138452, 1.0647107369924282, 1.4350845252893225, 0.5306282510621705, 1.4816045409242151, -0.2231435513142098, 1.7227665977411035, 1.2237754316221157, 1.7227665977411035, 0.9555114450274363, 1.0296194171811583, 2.1400661634962708, 1.2237754316221157, 1.1939224684724346, 2.163323025660538, 0.5877866649021191, 1.7578579175523736, 2.5726122302071057, 1.0296194171811583, 1.5686159179138452, 1.7404661748405044, 2.631888840136646, 2.0412203288596382, 1.7578579175523736, 1.5475625087160128, 2.0412203288596382, 0.9932517730102834, 1.5260563034950492, 1.791759469228055, 0.8329091229351041, 0.9162907318741552, 1.410986973710262, 1.5475625087160128, 1.5475625087160128, 2.3978952727983707, 2.0412203288596382, 1.1314021114911006, 0.4700036292457356, 0.5306282510621705, 2.8094026953624978, 1.1631508098056809, 1.6486586255873816, 1.6094379124341005, 1.8082887711792652, 0.0, 0.6418538861723948, 1.3862943611198906, 1.7404661748405044, -0.6931471805599453, 0.9932517730102834, 1.308332819650179, 1.840549633397487, 3.1654750481410856, 1.3862943611198906, 1.0986122886681098, 1.1314021114911006, 1.5686159179138452, 1.1314021114911006, 1.4586150226995167, 1.3609765531356006, 1.1314021114911006, 1.4816045409242151, 1.0986122886681098, 1.252762968495368, 2.151762203259462, 2.2082744135228043, 1.5892352051165808, 1.308332819650179, 0.8329091229351041, 1.0647107369924282, -0.1053605156578262, 0.4700036292457356, 1.5475625087160128, 1.3350010667323402, 1.308332819650179, 1.1314021114911006, 0.8329091229351041, 0.6931471805599453, 0.9932517730102834, 0.6418538861723948, 0.9162907318741552, 1.4816045409242151, 0.9932517730102834, 0.1823215567939548, 1.2237754316221157, 0.9555114450274363, 2.2512917986064958, 0.336472236621213, 2.1400661634962708, 1.62924053973028, 1.0986122886681098, 2.580216829592325, 2.734367509419584, 0.6418538861723948, 1.3609765531356006, 2.079441541679836, 0.9932517730102834, 2.4336133554004498, 1.4350845252893225, 2.517696472610991, 1.916922612182061, 1.9459101490553128, 1.5260563034950492, 0.0, 0.5877866649021191, 0.4054651081081644, 0.7419373447293773, 0.0953101798043249, 0.0953101798043249, 1.0647107369924282, 0.336472236621213, 2.4336133554004498, 2.778819271990417, 0.336472236621213, 0.336472236621213, 0.5306282510621705, 0.0, 1.0647107369924282, -0.5108256237659907, 0.4700036292457356, 1.9740810260220096, -0.5108256237659907, 2.322387720290225, 1.4816045409242151, 1.2237754316221157, 1.0986122886681098, 2.533696813957432, 1.4586150226995167, 1.5260563034950492, 1.3862943611198906, 1.2237754316221157, 2.8678989020441064, 2.3702437414678603, 2.079441541679836, 1.2809338454620642, 1.88706964903238, 1.9459101490553128, 1.6486586255873816, 2.4932054526026954, 1.6486586255873816, 2.19722457733622, 1.7749523509116736, 1.5475625087160128, 1.3862943611198906, 0.4700036292457356, 3.173878458937465, 0.0, 0.4054651081081644, 0.1823215567939548, 1.0647107369924282, 3.8774315606585272, 0.0, 2.128231705849268, 1.4350845252893225, -0.5108256237659907, 1.916922612182061, 2.028148247292285, 2.23001440015921, -0.5108256237659907, 0.4700036292457356, 2.341805806147327, 1.3862943611198906, 0.6418538861723948, 2.302585092994046, 0.8754687373538999, 1.5040773967762742, 1.0647107369924282, 0.1823215567939548, 0.262364264467491, 0.5306282510621705, 3.2386784521643803, -2.302585092994045, 2.3702437414678603, 0.8754687373538999, 1.3862943611198906, 1.9878743481543453, 0.7884573603642703, 1.1939224684724346, -0.5108256237659907, 1.7578579175523736, 0.4054651081081644, 0.7884573603642703, 1.5040773967762742, 0.9162907318741552, 1.6094379124341005, 1.1314021114911006, 1.1314021114911006, 1.0647107369924282, 1.3862943611198906, 2.3978952727983707, 1.8718021769015916, 0.7419373447293773, 1.1314021114911006, 1.5260563034950492, 0.7884573603642703, 2.091864061678393, 0.336472236621213, 2.23001440015921, 0.1823215567939548, 2.3702437414678603, 3.1822118404966093, 2.2192034840549946, 2.501435951739211, 2.1041341542702074, 2.388762789235098, 1.4586150226995167, 2.760009940032921, 1.7047480922384253, 1.840549633397487, 2.282382385676526, 2.1041341542702074, 0.5306282510621705, 0.5306282510621705, 1.8718021769015916, 1.5040773967762742, 2.424802725718295, 2.312535423847214, 1.5260563034950492, 2.091864061678393, 0.8754687373538999, 1.1939224684724346, 1.62924053973028, 1.4350845252893225, 0.1823215567939548, 0.7419373447293773, 0.1823215567939548, 1.0986122886681098, 0.7884573603642703, 2.066862759472976, 1.3609765531356006, 0.9555114450274363, 1.0986122886681098, 0.5877866649021191, 0.9555114450274363, 2.2512917986064958, -0.3566749439387324, 1.0296194171811583, 0.1823215567939548, 0.7884573603642703, 2.4932054526026954, 2.5416019934645457, 1.1939224684724346, 1.4586150226995167, 1.3609765531356006, 1.3350010667323402, 1.7749523509116736, -0.916290731874155, 1.4350845252893225, 1.0647107369924282, 0.6931471805599453, 0.262364264467491, 0.262364264467491, 0.4700036292457356, 2.2512917986064958, 0.5877866649021191, 2.501435951739211, 1.4816045409242151, 1.9459101490553128, 0.4054651081081644, 0.9555114450274363, 2.272125885509337, 1.3609765531356006, 1.252762968495368, 1.9315214116032136, 1.308332819650179, 0.8329091229351041, 0.9932517730102834, 0.7884573603642703, 1.9600947840472696, 0.262364264467491, 1.3609765531356006, 1.2809338454620642, 1.4586150226995167, 0.5306282510621705, 1.0647107369924282, 2.163323025660538, 1.840549633397487, 1.667706820558076, 1.0296194171811583, 0.262364264467491, 1.2809338454620642, 1.7227665977411035, 2.322387720290225, 1.7227665977411035, 0.262364264467491, 1.6094379124341005, 1.410986973710262, 1.2809338454620642, 0.9555114450274363, 0.262364264467491, 1.0296194171811583, 0.5877866649021191, 1.1631508098056809, -0.2231435513142098, 0.0953101798043249, 0.6931471805599453, 1.3609765531356006, 2.19722457733622, 2.0149030205422647, 3.0349529867072724, 1.8082887711792652, 0.7884573603642703, 1.7749523509116736, 2.282382385676526, 1.8718021769015916, 1.5475625087160128, 1.7404661748405044, 2.9496883350525844, 0.9162907318741552, 1.1314021114911006, 1.6486586255873816, 2.0541237336955462, 2.1041341542702074, 1.5686159179138452, 2.1400661634962708, 0.5306282510621705, 1.8082887711792652, 0.1823215567939548, 2.4423470353692043, 1.4816045409242151, 1.308332819650179, 2.341805806147327, 1.252762968495368, 1.1631508098056809, 1.308332819650179, 1.0296194171811583, 1.410986973710262, 0.262364264467491, 0.5877866649021191, 1.4586150226995167, 2.9652730660692828, 2.2192034840549946, 0.7419373447293773, 2.4423470353692043, 2.33214389523559, 0.7884573603642703, 0.262364264467491, 1.1939224684724346, 0.7419373447293773, 1.4816045409242151, 0.8329091229351041, 1.7047480922384253, 3.2308043957334744, 1.6486586255873816, 0.8754687373538999, 1.1939224684724346, 0.9555114450274363, 1.0647107369924282, 1.1631508098056809, 0.5306282510621705, 1.5686159179138452, 1.410986973710262, 1.62924053973028, 0.4700036292457356, 1.5892352051165808, -0.1053605156578262, -0.5108256237659907, 0.9162907318741552, 0.8754687373538999, 1.5475625087160128, 2.4069451083182885, 2.70805020110221, 2.163323025660538, 1.5260563034950492, 0.4700036292457356, 1.3862943611198906, 0.6418538861723948, 0.5306282510621705, -0.5108256237659907, -0.6931471805599453, -0.5108256237659907, 2.1747517214841605, 0.5306282510621705, 0.4054651081081644, 2.1747517214841605, 2.4159137783010487, 0.4700036292457356, 0.1823215567939548, 0.0, -0.2231435513142098, 1.4586150226995167, 1.252762968495368, 0.7884573603642703, 1.0986122886681098, 0.6418538861723948, 0.6418538861723948, 0.9162907318741552, 0.5877866649021191, -0.1053605156578262, 2.468099531471619, 0.6418538861723948, 1.0647107369924282, 1.2809338454620642, 1.308332819650179, 1.2809338454620642, 1.1314021114911006, 1.1939224684724346, 1.1631508098056809, 1.2237754316221157, 0.5877866649021191, 1.7404661748405044, 1.252762968495368, 0.4700036292457356, 3.475067230228611, 0.1823215567939548, 0.7884573603642703, -0.1053605156578262, 0.4700036292457356, 0.336472236621213, 1.1631508098056809, 1.9878743481543453, 0.4054651081081644, 0.336472236621213, 0.4700036292457356, 1.62924053973028, 0.8754687373538999, 0.9162907318741552, 0.262364264467491, 1.7047480922384253, 0.1823215567939548, 0.4054651081081644, 1.9878743481543453, 0.1823215567939548, 1.2237754316221157, 1.1939224684724346, 0.4700036292457356, 1.308332819650179, -0.1053605156578262, 0.5306282510621705, 0.4054651081081644, 1.0296194171811583, 1.2237754316221157, 0.0, -0.3566749439387324, 0.7419373447293773, 0.6931471805599453, 0.0, 1.7047480922384253, 0.4700036292457356, 1.1631508098056809, 0.6418538861723948, 0.0, 1.2237754316221157, 0.5877866649021191, 1.1631508098056809, -0.2231435513142098, 1.4816045409242151, 0.4054651081081644, 0.6418538861723948, 0.4700036292457356, 0.8329091229351041, 0.9162907318741552, 1.0296194171811583, 0.5877866649021191, 0.1823215567939548, 0.6418538861723948, -1.203972804325936, 0.8329091229351041, 1.5475625087160128, 0.7884573603642703, 0.7419373447293773, -0.2231435513142098, 1.8718021769015916, 1.1314021114911006, 0.7419373447293773, 0.0, 1.2237754316221157, 0.6418538861723948, 0.6418538861723948, 0.8329091229351041, 1.4816045409242151, 2.028148247292285, 1.8718021769015916, 2.128231705849268, 0.7884573603642703, 1.2237754316221157, 0.336472236621213, 1.62924053973028, 0.0953101798043249, 1.9600947840472696, 1.7578579175523736, 2.322387720290225, 1.9021075263969205, 0.9932517730102834, 1.2237754316221157, 0.4700036292457356, 1.62924053973028, 2.0149030205422647, 2.681021528714291, 0.6418538861723948, 2.0149030205422647, 0.9932517730102834, 1.3350010667323402, 0.6931471805599453, 0.8329091229351041, 1.62924053973028, 2.001480000210124, 1.3350010667323402, 1.0986122886681098, 1.5040773967762742, 2.1400661634962708, 1.6486586255873816, 1.308332819650179, 0.4700036292457356, 2.163323025660538, 2.3702437414678603, 2.091864061678393, 1.5260563034950492, 1.1314021114911006, 0.9162907318741552, 0.4700036292457356, 1.5892352051165808, 1.9315214116032136, 0.7884573603642703, 1.8082887711792652, 1.0986122886681098, 1.916922612182061, 2.9652730660692828, 1.410986973710262, 1.791759469228055, 2.2082744135228043, 2.1400661634962708, 0.1823215567939548, 1.1631508098056809, 2.451005098112319, 2.272125885509337, 1.0986122886681098, -0.2231435513142098, 1.1939224684724346, 1.5686159179138452, 1.5892352051165808, -0.6931471805599453, 2.2407096892759584, 0.5877866649021191, 0.0, 2.33214389523559, 2.0541237336955462, 0.8329091229351041, 1.88706964903238, 2.509599262378372, 1.5475625087160128, 1.840549633397487, 1.88706964903238, 1.0647107369924282, 0.6931471805599453, 0.262364264467491, 0.9162907318741552, 0.0953101798043249, 0.262364264467491, 0.5306282510621705, -0.1053605156578262, 0.5877866649021191, 1.5686159179138452, 0.5877866649021191, 1.2237754316221157, -0.1053605156578262, 2.2925347571405443, 1.6863989535702286, 2.151762203259462, 0.6931471805599453, 1.9021075263969205, 1.3609765531356006, 1.791759469228055, 1.6094379124341005, 0.9555114450274363, 2.379546134130174, 0.9162907318741552, 0.7884573603642703, 1.5686159179138452, 1.3350010667323402, 2.602689685444384, 1.0986122886681098, 1.4816045409242151, 1.3609765531356006, 0.6418538861723948, 0.4700036292457356, 0.6418538861723948, 0.336472236621213, 1.9021075263969205, 3.0204248861443626, 1.8082887711792652, 2.631888840136646, 2.33214389523559, 1.7578579175523736, 2.2407096892759584, 1.252762968495368, 1.4350845252893225, 2.4595888418037104, 1.9878743481543453, 1.5686159179138452, 0.6418538861723948, -0.2231435513142098, 1.5686159179138452, 2.33214389523559, 2.4336133554004498, 2.0412203288596382, 2.476538400117484, -0.5108256237659907, 1.916922612182061, 1.6863989535702286, 1.1631508098056809, 0.7884573603642703, 2.001480000210124, 1.6486586255873816, 0.8329091229351041, 0.8754687373538999, 2.772588722239781, 2.26176309847379, 1.8718021769015916, 1.5260563034950492, 1.62924053973028, 1.3350010667323402, 1.0986122886681098]

countyNames :: [String]
countyNames = [" AITKIN"," ANOKA"," BECKER"," BELTRAMI"," BENTON"," BIG STONE"," BLUE EARTH"," BROWN"," CARLTON"," CARVER"," CASS"," CHIPPEWA"," CHISAGO"," CLAY"," CLEARWATER"," COOK"," COTTONWOOD"," CROW WING"," DAKOTA"," DODGE"," DOUGLAS"," FARIBAULT"," FILLMORE"," FREEBORN"," GOODHUE"," HENNEPIN"," HOUSTON"," HUBBARD"," ISANTI"," ITASCA"," JACKSON"," KANABEC"," KANDIYOHI"," KITTSON"," KOOCHICHING"," LAC QUI PARLE"," LAKE"," LAKE OF THE WOODS"," LE SUEUR"," LINCOLN"," LYON"," MAHNOMEN"," MARSHALL"," MARTIN"," MCLEOD"," MEEKER"," MILLE LACS"," MORRISON"," MOWER"," MURRAY"," NICOLLET"," NOBLES"," NORMAN"," OLMSTED"," OTTER TAIL"," PENNINGTON"," PINE"," PIPESTONE"," POLK"," POPE"," RAMSEY"," REDWOOD"," RENVILLE"," RICE"," ROCK"," ROSEAU"," SCOTT"," SHERBURNE"," SIBLEY"," ST LOUIS"," STEARNS"," STEELE"," STEVENS"," SWIFT"," TODD"," TRAVERSE"," WABASHA"," WADENA"," WASECA"," WASHINGTON"," WATONWAN"," WILKIN"," WINONA"," WRIGHT"," YELLOW MEDICINE" ]

-- length = 919
countyIdx :: [Int]
countyIdx =
  [0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,3,3,3,3,3,3,3,4,4,4,4,5,5,5,6,6,6,6,6,6,6,6,6,6,6,6,6,6,7,7,7,7,8,8,8,8,8,8,8,8,8,8,9,9,9,9,9,9,10,10,10,10,10,11,11,11,11,12,12,12,12,12,12,13,13,13,13,13,13,13,13,13,13,13,13,13,13,14,14,14,14,15,15,16,16,16,16,17,17,17,17,17,17,17,17,17,17,17,17,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,19,19,19,20,20,20,20,20,20,20,20,20,21,21,21,21,21,21,22,22,23,23,23,23,23,23,23,23,23,24,24,24,24,24,24,24,24,24,24,24,24,24,24,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,26,26,26,26,26,26,27,27,27,27,27,28,28,28,29,29,29,29,29,29,29,29,29,29,29,30,30,30,30,30,31,31,31,31,32,32,32,32,33,33,33,34,34,34,34,34,34,34,35,35,36,36,36,36,36,36,36,36,36,37,37,37,37,38,38,38,38,38,39,39,39,39,40,40,40,40,40,40,40,40,41,42,42,42,42,42,42,42,42,42,43,43,43,43,43,43,43,44,44,44,44,44,44,44,44,44,44,44,44,44,45,45,45,45,45,46,46,47,47,47,47,47,47,47,47,47,48,48,48,48,48,48,48,48,48,48,48,48,48,49,50,50,50,50,51,51,51,52,52,52,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,54,54,54,54,54,54,54,54,55,55,55,56,56,56,56,56,56,57,57,57,57,58,58,58,58,59,59,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,61,61,61,61,61,62,62,62,63,63,63,63,63,63,63,63,63,63,63,64,64,65,65,65,65,65,65,65,65,65,65,65,65,65,65,66,66,66,66,66,66,66,66,66,66,66,66,66,67,67,67,67,67,67,67,67,68,68,68,68,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,71,71,71,71,71,71,71,71,71,71,72,72,73,73,73,73,74,74,74,75,75,75,75,76,76,76,76,76,76,76,77,77,77,77,77,78,78,78,78,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,80,80,80,81,82,82,82,82,82,82,82,82,82,82,82,82,82,83,83,83,83,83,83,83,83,83,83,83,83,83,84,84]

-- length = 919
dataFloorValues :: [Int]
dataFloorValues =
  [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0]
