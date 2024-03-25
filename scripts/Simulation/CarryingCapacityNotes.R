#Carrying capacity calculations: 
#original Sali: 
# estimated from Pollock et al. 2021 abundance of 200 Sali on Coco's island

#based on Sier's et al., 2024 updated area of Coco's
# New Coco's area: 33.6 ha
saliperha_new<-200/33.6
#area of HMU (ha)
hmu_area<-55
sali_hmu_K_new<-round(saliperha_new*hmu_area)
#area of refuge (ha) 
ref_area<-155.8
sali_ref_K_new<-round(saliperha_new*ref_area)



#for Koko
# 2 Koko per ha
# simply:
koko_K_hmu<-2*hmu_area
koko_K_ref<-round(2*ref_area)
