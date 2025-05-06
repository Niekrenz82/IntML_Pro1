# IntML_Pro1
Cooked By öglu.


test

# plots

When saving plots use:

ggsave(file = ".../.../name.pdf", height = x, width = y)

# Notes

## 04-05-2025 

- Jeg har lavet setup fil, der loader data m.m
- Transformeret grunddata
- Hvordan håndtere vi at rækker ikke er aufhængihngige af hinanden?
  - ID giver mulighed for at aggregerer på tværs af rækker
- Type_risk har stor betydning for de andre variable
- Skal vi bare fjerne length, højt korreleret med Weight på Bil og AGW.
- Det samme med Dist_channel.

## Spørgsmål Jin

- Hvilke variable har vi adgang til ved kontrakt statrt
  - Vel ikke N_claim_year, og Lapse_date osv.
Hvordan skal vi fortolke N_claims_year, der er eksempler på group_by(ID) %>% sum(N_claims_year) er større end N_claims_history.
- Giver Exposure mening, dækker kontrakten stadigt hvis Data_lapse < Date_next_renewal?



test