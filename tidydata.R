?xaringan::moon_reader

setouts1819 %>%
    tidyr::extract(`SCH.`, into = c("DOCe"),  "DOC-(\\d{1,2}.\\d{1,2}.\\d{2})"  , remove = FALSE  ) %>%
    tidyr::extract(`SCH.`, into = c("POHe"),  "POH-(\\d{1,2}.\\d{1,2}.\\d{2})"  , remove = FALSE  ) %>% 
    tidyr::extract(`SCH.`, into = c("M24e"),  "(M24-\\d{2}.\\d{2}.\\d{2})"      , remove = FALSE  ) %>% 
    tidyr::extract(`SCH.`, into = c("Monthe"),"(M\\d{1,2}-\\d{2}.\\d{2}.\\d{2})", remove = FALSE  ) %>% 
    tidyr::extract(`SCH.`, into = c("Te"),  "(T\\d{0,3}-\\d{2}.\\d{2}.\\d{2})"  , remove = FALSE  ) %>% 
    mutate( DOC = dmy(DOCe)   )      %>% 
    mutate(POH = dmy(POH)  )   %>%
    mutate( M24 = dmy(`M24/M72/M48/6Y`)  ) %>% 
    mutate( T90 = dmy(`T/T90`)  ) %>% 
    mutate( T   = dmy(`T/T90`)  ) %>% 
    mutate( Td   = dmy( str_sub(Te,-8,-1))    ) %>% 
    mutate(DOCdays = Date - DOC)   %>%
    mutate(Tdays = Date - Td )   %>%
    select(`Loco no.`, `SCH.`,DOC, DOCe, DOCdays,   T, Tdays,
           POH, M24,M24e,   T90, Monthe, Te,  everything())   ->  Td #    %>%     View()
glimpse(Td)