{- haspuyo.hs
   a tool to make ps(puyos) burst just once -}

import List
import Char

main = getContents >>= putStr . format . fall . brst . blocktize . puyotize

puyotize = locate . filter (not.isControl)
  where locate = zip [(x, y)| y<-[0..15], x<-[0..5]]

blocktize ps = let p:_ = colored ps in seperate [p] ps
  where seperate refs [] = [refs]
        seperate refs rems@(p:ps) = let neighbours = filter (neighbourOf refs) rems
          in if null neighbours then refs:(seperate [p] ps)
                     else seperate (refs++neighbours) (rems\\neighbours)

brst blocks = concatMap clearif blocks
  where clearif b = if length b > 3 then clear b else b

fall = (concatMap squash) . columns
  where squash col = let x = (fst.fst.head) col
                         coloredcs = getColorString (colored col)
                         squashed  = replicate (16 - length coloredcs) ' ' ++ coloredcs
                     in zip [(x, y)|y<-[0..15]] squashed

format = unlines . (map getColorString) . rows


-- utility functions --

columns ps = map (select fst ps) [0..5]
rows    ps = map (select snd ps) [0..15]
select f ps i = filter ((== i). f . fst) ps

colored ps = filter ((/= ' ') . snd) ps
clear   ps = map (\p->(fst p, ' ')) ps
getColorString ps = map snd ps

neighbourOf ps p = any (isAdjacentTo p) ps
isAdjacentTo p q = geoAdjacent (fst p) (fst q) && snd p == snd q
geoAdjacent (a,b) (c,d) = (abs(a-c)+abs(b-d)<=1)