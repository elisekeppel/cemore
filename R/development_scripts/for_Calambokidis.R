# HW sightings data for John Calambokidis
data <- readRDS("survey_data/cemore_survey_data_Sept2020.rds")
s <- data$sightings %>% 
  filter(photos == T, !species %in% c("Killer Whale", "Harbour Porpoise")) %>% 
  select(time_index..pdt., sgt.id, species, best.cnt, photos, comments) %>% 
  mutate(camera = NA, photo.frame.numbers = NA, detail = NA)
# s <- s[c(2:30),]
s$camera[c(1:27)] = c("m", "m", "m", "m", "m", "m", "m", "m", "m", "m/b", "m/b", "m/b", "m/b", "m/b", "m/b", "m/b", "m/b", "m/b", "m/b", "m/b", "m/b", "m/b", "m/b", "m/b", "m/b", "none")
s$photo.frame.numbers[c(1:27)] = c("0247, 0248", "0260, 0262", "0269", "0279, 0281", "0295", "0299, 0306", "0319, 0321, 0326, 0328", "0329, 0330, 0335, 0337, 0338, 0340, 0345, 0347, 0355, 0357, 0373, 0375","0384, 0386, 0397, 0404, 0408, 0409, 0411, 0414", "0475, 4990/91", "0488, 0503, 0525, 5002, 5010, 5019, 5020", "0567/71, 0578/83, 0589, 0594, 5021", "6013, 5028", "0620, 5031", "0625, 5035", "0656/59/70", "0679/82/85/86", "0695/97", "0719/21", "0737/41/46, 0748/51", "0763/79", "0819", "0826/31/34/38", "none", "none", "0882/84/92", "none")
s$detail[c(1:27)] = c("2 animals, dorsal fins", "same 2 animals as sgt.id S2, dorsal fins, one tagged", "same 2 animals as sgt.id S2", "same 2 animals as sgt.id S2", "dorsal fin, possibly different from previous sgt.", "dorsal fins, two animals, possibly different from previous sgt.", "4 fluke shots, 2 usable", "dorsal fins, 38 & 40 other direction for 29 & 30, 44 different fluke than 35 & 37, 55 & 57 & 73 & 75 may be new whales", "dorsal fins, 04 fluke not from first three dorsals, 08/09&11&14 fluke not sure whose", "side fluke", "88 tagged whale dorsal and accompanying whale fluke, 5002/19 tagged dorsal/fluke, 0503 tagged whale fluke, 25 close tag photo, 4990/91 poor fluke, 5010 other fluke, 5020 other dorsal", "67/71 dorsal/side fluke, 78/83 dorsal/dark fluke, 94 dorsal skin sloughing? 99 side fluke", "13 bright fluke", "0620 fluke, 5031 fluke", "0625 fluke, 5035 fluke", "SMOKE, partial fluke (wobbly), dorsal", "SMOKE, dorsal, dimple, fluke", "SMOKE, dorsal w dimple, partial fluke", "SMOKE, partial fluke, dorsal", "dorsal, fluke edge", "dorsal", "fluke", "fluke, dorsal", "too smokey", "too smokey", "2 dorsals/1 fluke", "")

write.csv(s, "output/HW_sightings_w_photos_Sept2020.csv")
