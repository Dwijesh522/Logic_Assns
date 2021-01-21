/*
* Colorlines: yellowLine, greenLine, greenbranchLine, 
*             magentaLine, pinkLine, orangeLine, 
*             bluebranchLine, greyLine, blueLine, 
*             redLine, violetLine
*/

/* whether two nodes are connected or not*/
connected(N1, N2):-
  edge(N1, N2);/*or*/
  edge(N2, N1).

% Get Cth answer of Goal
% Returns false if Cth ans doesn't exist
% Reference: https://stackoverflow.com/questions/11364634/count-the-number-of-calls-of-a-clause/11400256#11400256
call_nth(Goal, C):-
  State = count(0, _),
  Goal,
  arg(1, State, C1),
  C2 is C1 + 1,
  nb_setarg(1, State, C2),
  C = C2.

% Finds atmost N solutions of Goal 
% and store them in Instances
% References: https://stackoverflow.com/questions/20852980/prolog-findall-for-limited-number-of-solutions/20866206#20866206
/*
* How to use: Assume the following relations
* 
  friends(dwijesh, karan).
  friends(dwijesh, harsh).
  friends(dwijesh, harkirat).
  friends(harkirat, karan).

  Call callFirstN relation using the following
  findFirstN(5, Who, friends(dwijesh, Who), Instances).
*/
findFirstN(NState, Template, Goal, Instances):-
  arg(1, NState, N),
  findall(Template, callFirstN(Goal, N), Instances).

callFirstN(Goal, N):-
  call_nth(Goal, Nth),
  (Nth == N -> ! ; true).

/*
* Find one shortest Path from 
* X to Y at Depth == Bound
*/
% leaf node
shortestPath(X, Y, Depth, BoundState, Visited, Path):-
  arg(1, BoundState, Bound),
  Depth == Bound,
  X == Y,
  reverse([Y|Visited], Path).
% intermediate node
shortestPath(X, Y, Depth, BoundState, Visited, Path):-
  arg(1, BoundState, Bound),
  Depth \== Bound,
  connected(X, Z),
  (\+(Bound is Depth + 1) -> (Z = (C, S)) ; true),
  \+ member(Z, Visited), % no cycles
  NewDepth is Depth + 1,
  shortestPath(Z, Y, NewDepth, BoundState, [X|Visited], Path).

/*
* find atmost N shortest Path between X, Y
* Following function is linear function
*/
nPaths(N, X, Y, LL):-
  /* variable initialization*/
  Bound = count(0, _),
  Solns = count(N, _),
  AllPaths = count([], _),
  /* loop */
  repeat,
  findFirstN(Solns, Path, shortestPath(X, Y, 0, Bound, [], Path), Paths),
  /* updating variables*/
  length(Paths, FoundSolns),
  arg(1, Solns, OldSolns),
  arg(1, Bound, OldBound),
  arg(1, AllPaths, OldAllPaths),
  RemainingSolns is OldSolns - FoundSolns,
  NewBound is OldBound + 1,
  append(OldAllPaths, Paths, NewAllPaths),
  nb_setarg(1, AllPaths, NewAllPaths),
  nb_setarg(1, Bound, NewBound),
  nb_setarg(1, Solns, RemainingSolns),
  /* loop termination condition*/
  RemainingSolns = 0,
  LL = NewAllPaths.
  %  printPaths(LL).


% remove terminal stations from all paths
removeTerminal([H|[]], []).
removeTerminal([H|[HH|T]], Result):-
  removeTerminal([HH|T], NewResult),
  Result = [H|NewResult].

removeTerminalStations([[]], []).
removeTerminalStations([], []).
removeTerminalStations([HL|TL], NewResult):-
  [HT|T] = HL,
  removeTerminal(T, H),
  removeTerminalStations(TL, Result),
  NewResult = [H|Result].

paths(X, Y, LL):-
  State = count(1, _),
  findFirstN(State, L, nPaths(5, X, Y, L), [Result]),
  removeTerminalStations(Result, NewResult),
  NewResult = LL,
  printPaths(LL).

/*
* print paths found
*/
printList([]).
printList([H|T]):-
  write("("), write(H), write("), "), printList(T).

printPaths([[]]).
printPaths([HL|TL]):-
  length(HL, Len),
  write("Length: "), write(Len), nl, write("Path: "), 
  printList(HL), nl, nl,
  printPaths(TL).

/*  ----------------------------  dataset edges ------------------------------*/
edge((yellow, samaypurBadli), (yellow, rohiniSector18)).
edge((yellow, rohiniSector18), (yellow, haiderpur)).
edge((yellow, haiderpur), (yellow, jahangirpuri)).
edge((yellow, jahangirpuri), (yellow, adarshNagar)).
edge((yellow, adarshNagar), (yellow, azadpur)).
edge((yellow, azadpur), (yellow, modelTown)).
edge((yellow, modelTown), (yellow, guruTeghBahadurNagar)).
edge((yellow, guruTeghBahadurNagar), (yellow, vishwaVidyalaya)).
edge((yellow, vishwaVidyalaya), (yellow, vidhanSabha)).
edge((yellow, vidhanSabha), (yellow, civilLines)).
edge((yellow, civilLines), (yellow, kashmereGate)).
edge((yellow, kashmereGate), (yellow, chandniChowk)).
edge((yellow, chandniChowk), (yellow, chawriBazar)).
edge((yellow, chawriBazar), (yellow, newDelhi)).
edge((yellow, newDelhi), (yellow, rajivChowk)).
edge((yellow, rajivChowk), (yellow, patelChowk)).
edge((yellow, patelChowk), (yellow, centralSecretariat)).
edge((yellow, centralSecretariat), (yellow, udyogBhawan)).
edge((yellow, udyogBhawan), (yellow, lokKalyanMarg)).
edge((yellow, lokKalyanMarg), (yellow, jorbagh)).
edge((yellow, jorbagh), (yellow, dilliHaatINA)).
edge((yellow, dilliHaatINA), (yellow, aiims)).
edge((yellow, aiims), (yellow, greenPark)).
edge((yellow, greenPark), (yellow, hauzKhas)).
edge((yellow, hauzKhas), (yellow, malviyaNagar)).
edge((yellow, malviyaNagar), (yellow, saket)).
edge((yellow, saket), (yellow, qutabMinar)).
edge((yellow, qutabMinar), (yellow, chhatarpur)).
edge((yellow, chhatarpur), (yellow, sultanpur)).
edge((yellow, sultanpur), (yellow, ghitorni)).
edge((yellow, ghitorni), (yellow, arjanGarh)).
edge((yellow, arjanGarh), (yellow, guruDronacharya)).
edge((yellow, guruDronacharya), (yellow, sikandarpur)).
edge((yellow, sikandarpur), (yellow, mgRoad)).
edge((yellow, mgRoad), (yellow, iffcoChowk)).
edge((yellow, iffcoChowk), (yellow, hudaCityCentre)).

edge((green, inderlok), (green, ashokParkMain)).
edge((green, ashokParkMain), (green, punjabiBagh)).
edge((green, punjabiBagh), (green, shivajiPark)).
edge((green, shivajiPark), (green, madipur)).
edge((green, madipur), (green, paschimViharEast)).
edge((green, paschimViharEast), (green, paschimViharWest)).
edge((green, paschimViharWest), (green, peeraGarhi)).
edge((green, peeraGarhi), (green, udyogNagar)).
edge((green, udyogNagar), (green, maharajaSurajmalStadium)).
edge((green, maharajaSurajmalStadium), (green, nangloi)).
edge((green, nangloi), (green, nangloiRailwayStation)).
edge((green, nangloiRailwayStation), (green, rajdhaniPark)).
edge((green, rajdhaniPark), (green, mundka)).
edge((green, mundka), (green, mundkaIndustrialArea)).
edge((green, mundkaIndustrialArea), (green, ghevra)).
edge((green, ghevra), (green, tikriKalan)).
edge((green, tikriKalan), (green, tikriBorder)).
edge((green, tikriBorder), (green, panditShreeRamSharma)).
edge((green, panditShreeRamSharma), (green, bahadurgarhCity)).
edge((green, bahadurgarhCity), (green, brigadierHoshiyarSingh)).

edge((greenbranch, ashokParkMain), (greenbranch, satguruRamsinghMarg)).
edge((greenbranch, satguruRamsinghMarg), (greenbranch, kirtiNagar)).

edge((magenta, janakPuriWest), (magenta, dabriMor)).
edge((magenta, dabriMor), (magenta, dashrathPuri)).
edge((magenta, dashrathPuri), (magenta, palam)).
edge((magenta, palam), (magenta, sadarBazaarCantonment)).
edge((magenta, sadarBazaarCantonment), (magenta, terminal1IGIAirport)).
edge((magenta, terminal1IGIAirport), (magenta, shankarVihar)).
edge((magenta, shankarVihar), (magenta, vasantVihar)).
edge((magenta, vasantVihar), (magenta, munirka)).
edge((magenta, munirka), (magenta, rkPuram)).
edge((magenta, rkPuram), (magenta, iitDelhi)).
edge((magenta, iitDelhi), (magenta, hauzKhas)).
edge((magenta, hauzKhas), (magenta, panchsheelPark)).
edge((magenta, panchsheelPark), (magenta, chiragDelhi)).
edge((magenta, chiragDelhi), (magenta, greaterKailash)).
edge((magenta, greaterKailash), (magenta, nehruEnclave)).
edge((magenta, nehruEnclave), (magenta, kalkajiMandir)).
edge((magenta, kalkajiMandir), (magenta, okhlaNSIC)).
edge((magenta, okhlaNSIC), (magenta, sukhdevVihar)).
edge((magenta, sukhdevVihar), (magenta, jamiaMilliaIslamia)).
edge((magenta, jamiaMilliaIslamia), (magenta, okhlaVihar)).
edge((magenta, okhlaVihar), (magenta, jasolaViharShaheenBagh)).
edge((magenta, jasolaViharShaheenBagh), (magenta, kalindiKunj)).
edge((magenta, kalindiKunj), (magenta, okhlaBirdSanctuary)).
edge((magenta, okhlaBirdSanctuary), (magenta, botanicalGarden)).

edge((pink, majlisPark), (pink, azadpur)).
edge((pink, azadpur), (pink, shalimarBagh)).
edge((pink, shalimarBagh), (pink, netajiSubhashPlace)).
edge((pink, netajiSubhashPlace), (pink, shakurpur)).
edge((pink, shakurpur), (pink, punjabiBaghWest)).
edge((pink, punjabiBaghWest), (pink, esiBasaiDarapur)).
edge((pink, esiBasaiDarapur), (pink, rajouriGarden)).
edge((pink, rajouriGarden), (pink, mayaPuri)).
edge((pink, mayaPuri), (pink, narainaVihar)).
edge((pink, narainaVihar), (pink, delhiCantt)).
edge((pink, delhiCantt), (pink, durgabaiDeshmukhSouthCampus)).
edge((pink, durgabaiDeshmukhSouthCampus), (pink, sirVishweshwaraiahMotiBagh)).
edge((pink, sirVishweshwaraiahMotiBagh), (pink, bhikajiCamaPlace)).
edge((pink, bhikajiCamaPlace), (pink, dilliHaatINA)).
edge((pink, dilliHaatINA), (pink, southExtension)).
edge((pink, southExtension), (pink, lajpatNagar)).
edge((pink, lajpatNagar), (pink, vinobapuri)).
edge((pink, vinobapuri), (pink, ashram)).
edge((pink, ashram), (pink, saraiKaleKhanHazratNizamuddin)).
edge((pink, saraiKaleKhanHazratNizamuddin), (pink, mayurVihar1)).
edge((pink, mayurVihar1), (pink, mayurViharPocket1)).
edge((pink, mayurViharPocket1), (pink, trilokpuriSanjayLake)).
edge((pink, trilokpuriSanjayLake), (pink, vinodNagarEast)).
edge((pink, vinodNagarEast), (pink, mandawali)).
edge((pink, mandawali), (pink, ipExtension)).
edge((pink, ipExtension), (pink, anandVihar)).
edge((pink, anandVihar), (pink, karkarduma)).
edge((pink, karkarduma), (pink, karkardumaCourt)).
edge((pink, karkardumaCourt), (pink, krishnaNagar)).
edge((pink, krishnaNagar), (pink, eastAzadNagar)).
edge((pink, eastAzadNagar), (pink, welcome)).
edge((pink, welcome), (pink, jaffrabad)).
edge((pink, jaffrabad), (pink, maujpur)).
edge((pink, maujpur), (pink, gokulpuri)).
edge((pink, gokulpuri), (pink, johriEnclave)).
edge((pink, johriEnclave), (pink, shivVihar)).

edge((orange, newDelhi), (orange, shivajiStadium)).
edge((orange, shivajiStadium), (orange, dhaulaKuan)).
edge((orange, dhaulaKuan), (orange, delhiAerocity)).
edge((orange, delhiAerocity), (orange, igiAirport)).
edge((orange, igiAirport), (orange, dwarkaSector21)).

edge((bluebranch, yamunaBank), (bluebranch, laxmiNagar)).
edge((bluebranch, laxmiNagar), (bluebranch, nirmanVihar)).
edge((bluebranch, nirmanVihar), (bluebranch, preetVihar)).
edge((bluebranch, preetVihar), (bluebranch, karkarduma)).
edge((bluebranch, karkarduma), (bluebranch, anandVihar)).
edge((bluebranch, anandVihar), (bluebranch, kaushambi)).
edge((bluebranch, kaushambi), (bluebranch, vaishali)).

edge((grey, dwarka), (grey, nangli)).
edge((grey, nangli), (grey, najafgarh)).

edge((blue, dwarkaSector21), (blue, dwarkaSector8)).
edge((blue, dwarkaSector8), (blue, dwarkaSector9)).
edge((blue, dwarkaSector9), (blue, dwarkaSector10)).
edge((blue, dwarkaSector10), (blue, dwarkaSector11)).
edge((blue, dwarkaSector11), (blue, dwarkaSector12)).
edge((blue, dwarkaSector12), (blue, dwarkaSector13)).
edge((blue, dwarkaSector13), (blue, dwarkaSector14)).
edge((blue, dwarkaSector14), (blue, dwarka)).
edge((blue, dwarka), (blue, dwarkaMor)).
edge((blue, dwarkaMor), (blue, nawada)).
edge((blue, nawada), (blue, uttamNagarWest)).
edge((blue, uttamNagarWest), (blue, uttamNagarEast)).
edge((blue, uttamNagarEast), (blue, janakPuriWest)).
edge((blue, janakPuriWest), (blue, janakPuriEast)).
edge((blue, janakPuriEast), (blue, tilakNagar)).
edge((blue, tilakNagar), (blue, subhashNagar)).
edge((blue, subhashNagar), (blue, tagoreGarden)).
edge((blue, tagoreGarden), (blue, rajouriGarden)).
edge((blue, rajouriGarden), (blue, rameshNagar)).
edge((blue, rameshNagar), (blue, motiNagar)).
edge((blue, motiNagar), (blue, kirtiNagar)).
edge((blue, kirtiNagar), (blue, shadipur)).
edge((blue, shadipur), (blue, patelNagar)).
edge((blue, patelNagar), (blue, rajendraPlace)).
edge((blue, rajendraPlace), (blue, karolBagh)).
edge((blue, karolBagh), (blue, jhandewalan)).
edge((blue, jhandewalan), (blue, rkAshramMarg)).
edge((blue, rkAshramMarg), (blue, rajivChowk)).
edge((blue, rajivChowk), (blue, barakhamba)).
edge((blue, barakhamba), (blue, mandiHouse)).
edge((blue, mandiHouse), (blue, supremeCourt)).
edge((blue, supremeCourt), (blue, indraprastha)).
edge((blue, indraprastha), (blue, yamunaBank)).
edge((blue, yamunaBank), (blue, akshardham)).
edge((blue, akshardham), (blue, mayurVihar1)).
edge((blue, mayurVihar1), (blue, mayurViharExtention)).
edge((blue, mayurViharExtention), (blue, newAshokNagar)).
edge((blue, newAshokNagar), (blue, noidaSector15)).
edge((blue, noidaSector15), (blue, noidaSector16)).
edge((blue, noidaSector16), (blue, noidaSector18)).
edge((blue, noidaSector18), (blue, botanicalGarden)).
edge((blue, botanicalGarden), (blue, golfCourse)).
edge((blue, golfCourse), (blue, noidaCityCenter)).
edge((blue, noidaCityCenter), (blue, noidaSector34)).
edge((blue, noidaSector34), (blue, noidaSector52)).
edge((blue, noidaSector52), (blue, noidaSector61)).
edge((blue, noidaSector61), (blue, noidaSector59)).
edge((blue, noidaSector59), (blue, noidaSector62)).
edge((blue, noidaSector62), (blue, noidaElectronicCity)).

edge((red, shaheedSthal), (red, hindonRiver)).
edge((red, hindonRiver), (red, arthala)).
edge((red, arthala), (red, mohanNagar)).
edge((red, mohanNagar), (red, shyamPark)).
edge((red, shyamPark), (red, majorMohitSharma)).
edge((red, majorMohitSharma), (red, rajBagh)).
edge((red, rajBagh), (red, shaheedNagar)).
edge((red, shaheedNagar), (red, dilshadGarden)).
edge((red, dilshadGarden), (red, jhilMil)).
edge((red, jhilMil), (red, mansaroverPark)).
edge((red, mansaroverPark), (red, shahdara)).
edge((red, shahdara), (red, welcome)).
edge((red, welcome), (red, seelampur)).
edge((red, seelampur), (red, shastriPark)).
edge((red, shastriPark), (red, kashmereGate)).
edge((red, kashmereGate), (red, tisHazari)).
edge((red, tisHazari), (red, pulBangash)).
edge((red, pulBangash), (red, pratapNagar)).
edge((red, pratapNagar), (red, shastriNagar)).
edge((red, shastriNagar), (red, inderlok)).
edge((red, inderlok), (red, kanhaiyaNagar)).
edge((red, kanhaiyaNagar), (red, keshavPuram)).
edge((red, keshavPuram), (red, netajiSubhashPlace)).
edge((red, netajiSubhashPlace), (red, kohatEnclave)).
edge((red, kohatEnclave), (red, pitamPura)).
edge((red, pitamPura), (red, rohiniEast)).
edge((red, rohiniEast), (red, rohiniWest)).
edge((red, rohiniWest), (red, rithala)).

edge((violet, kashmereGate), (violet, lalQuila)).
edge((violet, lalQuila), (violet, jamaMasjid)).
edge((violet, jamaMasjid), (violet, delhiGate)).
edge((violet, delhiGate), (violet, ito)).
edge((violet, ito), (violet, mandiHouse)).
edge((violet, mandiHouse), (violet, janpath)).
edge((violet, janpath), (violet, centralSecretariat)).
edge((violet, centralSecretariat), (violet, khanMarket)).
edge((violet, khanMarket), (violet, jawaharlalNehruStadium)).
edge((violet, jawaharlalNehruStadium), (violet, jangpura)).
edge((violet, jangpura), (violet, lajpatNagar)).
edge((violet, lajpatNagar), (violet, moolchand)).
edge((violet, moolchand), (violet, kailashColony)).
edge((violet, kailashColony), (violet, nehruPlace)).
edge((violet, nehruPlace), (violet, kalkajiMandir)).
edge((violet, kalkajiMandir), (violet, govindPuri)).
edge((violet, govindPuri), (violet, okhla)).
edge((violet, okhla), (violet, jasola)).
edge((violet, jasola), (violet, saritaVihar)).
edge((violet, saritaVihar), (violet, mohanEstate)).
edge((violet, mohanEstate), (violet, tughlakabad)).
edge((violet, tughlakabad), (violet, badarpurBorder)).
edge((violet, badarpurBorder), (violet, sarai)).
edge((violet, sarai), (violet, nhpcChowk)).
edge((violet, nhpcChowk), (violet, mewalaMaharajpur)).
edge((violet, mewalaMaharajpur), (violet, sector28Faridabad)).
edge((violet, sector28Faridabad), (violet, badkalMor)).
edge((violet, badkalMor), (violet, oldFaridabad)).
edge((violet, oldFaridabad), (violet, neelamChowkAjronda)).
edge((violet, neelamChowkAjronda), (violet, bataChowk)).
edge((violet, bataChowk), (violet, escortsMujesar)).
edge((violet, escortsMujesar), (violet, santSurdasSihi)).
edge((violet, santSurdasSihi), (violet, rajaNaharSingh)).

/*  ------------------------  same stations with diff color lines --------------------------------*/
edge((yellow, azadpur), (pink, azadpur)).
edge((yellow, kashmereGate), (red, kashmereGate)).
edge((yellow, kashmereGate), (violet, kashmereGate)).
edge((yellow, newDelhi), (orange, newDelhi)).
edge((yellow, rajivChowk), (blue, rajivChowk)).
edge((yellow, centralSecretariat), (violet, centralSecretariat)).
edge((yellow, dilliHaatINA), (pink, dilliHaatINA)).
edge((yellow, hauzKhas), (magenta, hauzKhas)).
edge((green, inderlok), (red, inderlok)).
edge((green, ashokParkMain), (greenbranch, ashokParkMain)).
edge((magenta, janakPuriWest), (blue, janakPuriWest)).
edge((magenta, kalkajiMandir), (violet, kalkajiMandir)).
edge((pink, netajiSubhashPlace), (red, netajiSubhashPlace)).
edge((pink, rajouriGarden), (blue, rajouriGarden)).
edge((pink, lajpatNagar), (violet, lajpatNagar)).
edge((pink, mayurVihar1), (blue, mayurVihar1)).
edge((pink, anandVihar), (bluebranch, anandVihar)).
edge((pink, karkarduma), (bluebranch, karkarduma)).
edge((pink, welcome), (red, welcome)).
edge((bluebranch, yamunaBank), (blue, yamunaBank)).
edge((grey, dwarka), (blue, dwarka)).
edge((blue, dwarkaSector21), (orange, dwarkaSector21)).
edge((blue, kirtiNagar), (greenbranch, kirtiNagar)).
edge((blue, mandiHouse), (violet, mandiHouse)).
edge((blue, botanicalGarden), (magenta, botanicalGarden)).
edge((red, kashmereGate), (violet, kashmereGate)).

/* -------------------------------  source and sink nodes --------------------------*/
edge(samaypurBadli, (yellow, samaypurBadli)).
edge(rohiniSector18, (yellow, rohiniSector18)).
edge(haiderpur, (yellow, haiderpur)).
edge(jahangirpuri, (yellow, jahangirpuri)).
edge(adarshNagar, (yellow, adarshNagar)).
edge(azadpur, (yellow, azadpur)).
edge(modelTown, (yellow, modelTown)).
edge(guruTeghBahadurNagar, (yellow, guruTeghBahadurNagar)).
edge(vishwaVidyalaya, (yellow, vishwaVidyalaya)).
edge(vidhanSabha, (yellow, vidhanSabha)).
edge(civilLines, (yellow, civilLines)).
edge(kashmereGate, (yellow, kashmereGate)).
edge(chandniChowk, (yellow, chandniChowk)).
edge(chawriBazar, (yellow, chawriBazar)).
edge(newDelhi, (yellow, newDelhi)).
edge(rajivChowk, (yellow, rajivChowk)).
edge(patelChowk, (yellow, patelChowk)).
edge(centralSecretariat, (yellow, centralSecretariat)).
edge(udyogBhawan, (yellow, udyogBhawan)).
edge(lokKalyanMarg, (yellow, lokKalyanMarg)).
edge(jorbagh, (yellow, jorbagh)).
edge(dilliHaatINA, (yellow, dilliHaatINA)).
edge(aiims, (yellow, aiims)).
edge(greenPark, (yellow, greenPark)).
edge(hauzKhas, (yellow, hauzKhas)).
edge(malviyaNagar, (yellow, malviyaNagar)).
edge(saket, (yellow, saket)).
edge(qutabMinar, (yellow, qutabMinar)).
edge(chhatarpur, (yellow, chhatarpur)).
edge(sultanpur, (yellow, sultanpur)).
edge(ghitorni, (yellow, ghitorni)).
edge(arjanGarh, (yellow, arjanGarh)).
edge(guruDronacharya, (yellow, guruDronacharya)).
edge(sikandarpur, (yellow, sikandarpur)).
edge(mgRoad, (yellow, mgRoad)).
edge(iffcoChowk, (yellow, iffcoChowk)).
edge(hudaCityCentre, (yellow, hudaCityCentre)).
edge(inderlok, (green, inderlok)).
edge(ashokParkMain, (green, ashokParkMain)).
edge(punjabiBagh, (green, punjabiBagh)).
edge(shivajiPark, (green, shivajiPark)).
edge(madipur, (green, madipur)).
edge(paschimViharEast, (green, paschimViharEast)).
edge(paschimViharWest, (green, paschimViharWest)).
edge(peeraGarhi, (green, peeraGarhi)).
edge(udyogNagar, (green, udyogNagar)).
edge(maharajaSurajmalStadium, (green, maharajaSurajmalStadium)).
edge(nangloi, (green, nangloi)).
edge(nangloiRailwayStation, (green, nangloiRailwayStation)).
edge(rajdhaniPark, (green, rajdhaniPark)).
edge(mundka, (green, mundka)).
edge(mundkaIndustrialArea, (green, mundkaIndustrialArea)).
edge(ghevra, (green, ghevra)).
edge(tikriKalan, (green, tikriKalan)).
edge(tikriBorder, (green, tikriBorder)).
edge(panditShreeRamSharma, (green, panditShreeRamSharma)).
edge(bahadurgarhCity, (green, bahadurgarhCity)).
edge(brigadierHoshiyarSingh, (green, brigadierHoshiyarSingh)).
edge(ashokParkMain, (greenbranch, ashokParkMain)).
edge(satguruRamsinghMarg, (greenbranch, satguruRamsinghMarg)).
edge(kirtiNagar, (greenbranch, kirtiNagar)).
edge(janakPuriWest, (magenta, janakPuriWest)).
edge(dabriMor, (magenta, dabriMor)).
edge(dashrathPuri, (magenta, dashrathPuri)).
edge(palam, (magenta, palam)).
edge(sadarBazaarCantonment, (magenta, sadarBazaarCantonment)).
edge(terminal1IGIAirport, (magenta, terminal1IGIAirport)).
edge(shankarVihar, (magenta, shankarVihar)).
edge(vasantVihar, (magenta, vasantVihar)).
edge(munirka, (magenta, munirka)).
edge(rkPuram, (magenta, rkPuram)).
edge(iitDelhi, (magenta, iitDelhi)).
edge(hauzKhas, (magenta, hauzKhas)).
edge(panchsheelPark, (magenta, panchsheelPark)).
edge(chiragDelhi, (magenta, chiragDelhi)).
edge(greaterKailash, (magenta, greaterKailash)).
edge(nehruEnclave, (magenta, nehruEnclave)).
edge(kalkajiMandir, (magenta, kalkajiMandir)).
edge(okhlaNSIC, (magenta, okhlaNSIC)).
edge(sukhdevVihar, (magenta, sukhdevVihar)).
edge(jamiaMilliaIslamia, (magenta, jamiaMilliaIslamia)).
edge(okhlaVihar, (magenta, okhlaVihar)).
edge(jasolaViharShaheenBagh, (magenta, jasolaViharShaheenBagh)).
edge(kalindiKunj, (magenta, kalindiKunj)).
edge(okhlaBirdSanctuary, (magenta, okhlaBirdSanctuary)).
edge(botanicalGarden, (magenta, botanicalGarden)).
edge(majlisPark, (pink, majlisPark)).
edge(azadpur, (pink, azadpur)).
edge(shalimarBagh, (pink, shalimarBagh)).
edge(netajiSubhashPlace, (pink, netajiSubhashPlace)).
edge(shakurpur, (pink, shakurpur)).
edge(punjabiBaghWest, (pink, punjabiBaghWest)).
edge(esiBasaiDarapur, (pink, esiBasaiDarapur)).
edge(rajouriGarden, (pink, rajouriGarden)).
edge(mayaPuri, (pink, mayaPuri)).
edge(narainaVihar, (pink, narainaVihar)).
edge(delhiCantt, (pink, delhiCantt)).
edge(durgabaiDeshmukhSouthCampus, (pink, durgabaiDeshmukhSouthCampus)).
edge(sirVishweshwaraiahMotiBagh, (pink, sirVishweshwaraiahMotiBagh)).
edge(bhikajiCamaPlace, (pink, bhikajiCamaPlace)).
edge(dilliHaatINA, (pink, dilliHaatINA)).
edge(southExtension, (pink, southExtension)).
edge(lajpatNagar, (pink, lajpatNagar)).
edge(vinobapuri, (pink, vinobapuri)).
edge(ashram, (pink, ashram)).
edge(saraiKaleKhanHazratNizamuddin, (pink, saraiKaleKhanHazratNizamuddin)).
edge(mayurVihar1, (pink, mayurVihar1)).
edge(mayurViharPocket1, (pink, mayurViharPocket1)).
edge(trilokpuriSanjayLake, (pink, trilokpuriSanjayLake)).
edge(vinodNagarEast, (pink, vinodNagarEast)).
edge(mandawali, (pink, mandawali)).
edge(ipExtension, (pink, ipExtension)).
edge(anandVihar, (pink, anandVihar)).
edge(karkarduma, (pink, karkarduma)).
edge(karkardumaCourt, (pink, karkardumaCourt)).
edge(krishnaNagar, (pink, krishnaNagar)).
edge(eastAzadNagar, (pink, eastAzadNagar)).
edge(welcome, (pink, welcome)).
edge(jaffrabad, (pink, jaffrabad)).
edge(maujpur, (pink, maujpur)).
edge(gokulpuri, (pink, gokulpuri)).
edge(johriEnclave, (pink, johriEnclave)).
edge(shivVihar, (pink, shivVihar)).
edge(newDelhi, (orange, newDelhi)).
edge(shivajiStadium, (orange, shivajiStadium)).
edge(dhaulaKuan, (orange, dhaulaKuan)).
edge(delhiAerocity, (orange, delhiAerocity)).
edge(igiAirport, (orange, igiAirport)).
edge(dwarkaSector21, (orange, dwarkaSector21)).
edge(yamunaBank, (bluebranch, yamunaBank)).
edge(laxmiNagar, (bluebranch, laxmiNagar)).
edge(nirmanVihar, (bluebranch, nirmanVihar)).
edge(preetVihar, (bluebranch, preetVihar)).
edge(karkarduma, (bluebranch, karkarduma)).
edge(anandVihar, (bluebranch, anandVihar)).
edge(kaushambi, (bluebranch, kaushambi)).
edge(vaishali, (bluebranch, vaishali)).
edge(dwarka, (grey, dwarka)).
edge(nangli, (grey, nangli)).
edge(najafgarh, (grey, najafgarh)).
edge(dwarkaSector21, (blue, dwarkaSector21)).
edge(dwarkaSector8, (blue, dwarkaSector8)).
edge(dwarkaSector9, (blue, dwarkaSector9)).
edge(dwarkaSector10, (blue, dwarkaSector10)).
edge(dwarkaSector11, (blue, dwarkaSector11)).
edge(dwarkaSector12, (blue, dwarkaSector12)).
edge(dwarkaSector13, (blue, dwarkaSector13)).
edge(dwarkaSector14, (blue, dwarkaSector14)).
edge(dwarka, (blue, dwarka)).
edge(dwarkaMor, (blue, dwarkaMor)).
edge(nawada, (blue, nawada)).
edge(uttamNagarWest, (blue, uttamNagarWest)).
edge(uttamNagarEast, (blue, uttamNagarEast)).
edge(janakPuriWest, (blue, janakPuriWest)).
edge(janakPuriEast, (blue, janakPuriEast)).
edge(tilakNagar, (blue, tilakNagar)).
edge(subhashNagar, (blue, subhashNagar)).
edge(tagoreGarden, (blue, tagoreGarden)).
edge(rajouriGarden, (blue, rajouriGarden)).
edge(rameshNagar, (blue, rameshNagar)).
edge(motiNagar, (blue, motiNagar)).
edge(kirtiNagar, (blue, kirtiNagar)).
edge(shadipur, (blue, shadipur)).
edge(patelNagar, (blue, patelNagar)).
edge(rajendraPlace, (blue, rajendraPlace)).
edge(karolBagh, (blue, karolBagh)).
edge(jhandewalan, (blue, jhandewalan)).
edge(rkAshramMarg, (blue, rkAshramMarg)).
edge(rajivChowk, (blue, rajivChowk)).
edge(barakhamba, (blue, barakhamba)).
edge(mandiHouse, (blue, mandiHouse)).
edge(supremeCourt, (blue, supremeCourt)).
edge(indraprastha, (blue, indraprastha)).
edge(yamunaBank, (blue, yamunaBank)).
edge(akshardham, (blue, akshardham)).
edge(mayurVihar1, (blue, mayurVihar1)).
edge(mayurViharExtention, (blue, mayurViharExtention)).
edge(newAshokNagar, (blue, newAshokNagar)).
edge(noidaSector15, (blue, noidaSector15)).
edge(noidaSector16, (blue, noidaSector16)).
edge(noidaSector18, (blue, noidaSector18)).
edge(botanicalGarden, (blue, botanicalGarden)).
edge(golfCourse, (blue, golfCourse)).
edge(noidaCityCenter, (blue, noidaCityCenter)).
edge(noidaSector34, (blue, noidaSector34)).
edge(noidaSector52, (blue, noidaSector52)).
edge(noidaSector61, (blue, noidaSector61)).
edge(noidaSector59, (blue, noidaSector59)).
edge(noidaSector62, (blue, noidaSector62)).
edge(noidaElectronicCity, (blue, noidaElectronicCity)).
edge(shaheedSthal, (red, shaheedSthal)).
edge(hindonRiver, (red, hindonRiver)).
edge(arthala, (red, arthala)).
edge(mohanNagar, (red, mohanNagar)).
edge(shyamPark, (red, shyamPark)).
edge(majorMohitSharma, (red, majorMohitSharma)).
edge(rajBagh, (red, rajBagh)).
edge(shaheedNagar, (red, shaheedNagar)).
edge(dilshadGarden, (red, dilshadGarden)).
edge(jhilMil, (red, jhilMil)).
edge(mansaroverPark, (red, mansaroverPark)).
edge(shahdara, (red, shahdara)).
edge(welcome, (red, welcome)).
edge(seelampur, (red, seelampur)).
edge(shastriPark, (red, shastriPark)).
edge(kashmereGate, (red, kashmereGate)).
edge(tisHazari, (red, tisHazari)).
edge(pulBangash, (red, pulBangash)).
edge(pratapNagar, (red, pratapNagar)).
edge(shastriNagar, (red, shastriNagar)).
edge(inderlok, (red, inderlok)).
edge(kanhaiyaNagar, (red, kanhaiyaNagar)).
edge(keshavPuram, (red, keshavPuram)).
edge(netajiSubhashPlace, (red, netajiSubhashPlace)).
edge(kohatEnclave, (red, kohatEnclave)).
edge(pitamPura, (red, pitamPura)).
edge(rohiniEast, (red, rohiniEast)).
edge(rohiniWest, (red, rohiniWest)).
edge(rithala, (red, rithala)).
edge(kashmereGate, (violet, kashmereGate)).
edge(lalQuila, (violet, lalQuila)).
edge(jamaMasjid, (violet, jamaMasjid)).
edge(delhiGate, (violet, delhiGate)).
edge(ito, (violet, ito)).
edge(mandiHouse, (violet, mandiHouse)).
edge(janpath, (violet, janpath)).
edge(centralSecretariat, (violet, centralSecretariat)).
edge(khanMarket, (violet, khanMarket)).
edge(jawaharlalNehruStadium, (violet, jawaharlalNehruStadium)).
edge(jangpura, (violet, jangpura)).
edge(lajpatNagar, (violet, lajpatNagar)).
edge(moolchand, (violet, moolchand)).
edge(kailashColony, (violet, kailashColony)).
edge(nehruPlace, (violet, nehruPlace)).
edge(kalkajiMandir, (violet, kalkajiMandir)).
edge(govindPuri, (violet, govindPuri)).
edge(okhla, (violet, okhla)).
edge(jasola, (violet, jasola)).
edge(saritaVihar, (violet, saritaVihar)).
edge(mohanEstate, (violet, mohanEstate)).
edge(tughlakabad, (violet, tughlakabad)).
edge(badarpurBorder, (violet, badarpurBorder)).
edge(sarai, (violet, sarai)).
edge(nhpcChowk, (violet, nhpcChowk)).
edge(mewalaMaharajpur, (violet, mewalaMaharajpur)).
edge(sector28Faridabad, (violet, sector28Faridabad)).
edge(badkalMor, (violet, badkalMor)).
edge(oldFaridabad, (violet, oldFaridabad)).
edge(neelamChowkAjronda, (violet, neelamChowkAjronda)).
edge(bataChowk, (violet, bataChowk)).
edge(escortsMujesar, (violet, escortsMujesar)).
edge(santSurdasSihi, (violet, santSurdasSihi)).
edge(rajaNaharSingh, (violet, rajaNaharSingh)).

/* 647 - 401 = 246
* Program to print all edges given the dataset
* 1) all consecutive stations in a given colorline are edges
* 2) Same station in different colorline are edges
*
* if station a has two colorlines c1, c2 then write 'edge((c1, a), (c2, a))' on STDOUT
*/

/*case 1: print consecutive stations in given colour line*/
printconsecutiveStations(/*input*/Color, [Head|[]]).
printConsecutiveStations(/*input*/Color, [Head|[Tail|TailList]]):-
  /* if station a, b exists in same colorline c then write 'edge((c, a), (c, b)).' on STDOUT*/
  write("edge("), print(Color, Head), write(", "), print(Color, Tail), write(")."), nl,
  printConsecutiveStations(/*input*/Color, [Tail|TailList]).

print(Color, Head):-
  /* prints on STDOUT: (Color, Head)*/
  write("("), write(Color), write(", "), write(Head), write(")").

/*case 2: print stations having different colorlines*/
printStationsDiffColorLines(Visited, Node1, Node3):-
  (Node1 == (red, kashmereGate), Node3 == (violet, kashmereGate));
  (connected((Color1, Station1), (Color2, Station2)),
  connected((Color3, Station3), (Color4, Station4)),
  Color1 \== Color3,
  Station1 == Station3,
  \+ member((Color1, Color3, Station1), Visited),
  write("edge("), print(Color1, Station1), write(", "), print(Color3, Station1), write(")."), nl,
  printStationsDiffColorLines([(Color1, Color3, Station1)|[(Color3, Color1, Station1)|Visited]], (Color1, Station1), (Color3, Station3))).

sourceSinkNodes(Visited):-
  member((violet, rajaNaharSingh), Visited).

sourceSinkNodes(Visited):- /*both node are not seen*/
  edge((C1, S1), (C2, S2)),
  ( \+ member((C1, S1), Visited),
    \+ member((C2, S2), Visited)  -> 
    ( write("edge("), write(S1), write(", "), print(C1, S1), write(")."), nl,
      write("edge("), write(S2), write(", "), print(C2, S2), write(")."), nl,
      sourceSinkNodes([(C2, S2)|[(C1, S1)|Visited]])
    );
    \+ member((C1, S1), Visited)  ->
    (
      write("edge("), write(S1), write(", "), print(C1, S1), write(")."), nl,
      sourceSinkNodes([(C1, S1)|Visited])
    );
    \+ member((C2, S2), Visited)  ->
    (
      write("edge("), write(S2), write(", "), print(C2, S2), write(")."), nl,
      sourceSinkNodes([(C2, S2)|Visited])
    );
    fail
  ).
