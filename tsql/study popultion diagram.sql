use [NHSE_Sandbox_StrategyUnit]

 --drop table [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_study_pop_deriv_start

select 
	der_pseudo_nhs_number,
	Der_Age_at_CDS_Activity_Date,
	sex,
	ethnic_group,
	[IndexofMultipleDeprivationIMDDecile] as imd2019Dec,
	[STP21CD],
	[STP21NM],
	der_spell_LoS,
	der_diagnosis_all,
	der_diagnosis_count,
	der_procedure_count,
	der_admit_treatment_function_code,
	der_national_pod_code,
	admission_date, 
	discharge_date,
	MAX(discharge_method) as discharge_method, 
	MAX(discharge_destination) as discharge_destination

	into [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_study_pop_deriv_start

	from NHSE_SUSPlus_Live.[dbo].[tbl_Data_SEM_APCS]

	left outer join [NHSE_Sandbox_Spec_Neurology].[dbo].[RefIMD2019]
	on NHSE_SUSPlus_Live.[dbo].[tbl_Data_SEM_APCS].Der_Postcode_LSOA_2011_Code = [NHSE_Sandbox_Spec_Neurology].[dbo].[RefIMD2019].[LSOAcode2011]

	left outer join [NHSE_Sandbox_StrategyUnit].[dbo].[LSOA(2011)_to_CCG_to_STP_(April_2021)]
	on NHSE_SUSPlus_Live.[dbo].[tbl_Data_SEM_APCS].Der_Postcode_LSOA_2011_Code = [NHSE_Sandbox_StrategyUnit].[dbo].[LSOA(2011)_to_CCG_to_STP_(April_2021)].[LSOA11CD]

	where 
	1=1
    --and der_age_at_cds_activity_date >= 65
	--and discharge_date  is not null
	and discharge_date >= '2022-06-01 00:00:00.000'
	and discharge_date <= '2022-07-31 00:00:00.000'
	--and discharge_method <> '4'
	--and discharge_destination = 19
	--and der_pseudo_nhs_number is not null
	--and Der_National_POD_Code <> 'RADAY'
	--and Der_National_POD_Code <> 'RANIGHT'
	--and der_spell_LoS <= 60

	group by
	der_pseudo_nhs_number,
	Der_Age_at_CDS_Activity_Date,
	sex,
	ethnic_group,
	[IndexofMultipleDeprivationIMDDecile],
	[STP21CD],
	[STP21NM],
	der_spell_LoS,
	der_diagnosis_all,
	der_diagnosis_count,
	der_procedure_count,
	der_admit_treatment_function_code,
	der_national_pod_code,
	admission_date, discharge_date






select '0_start' as stage, COUNT(*) as discharges
from [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_study_pop_deriv_start

union all

select '1_exclUnder65' as stage, COUNT(*) as discharges
from [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_study_pop_deriv_start
	where 
	1=1
    and der_age_at_cds_activity_date >= 65

union all

select '2_exclRegDN' as stage, COUNT(*) as discharges
from [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_study_pop_deriv_start
	where 
	1=1
    and der_age_at_cds_activity_date >= 65
	and Der_National_POD_Code <> 'RADAY'
	and Der_National_POD_Code <> 'RANIGHT'

union all

select '3_exclNotHome' as stage, COUNT(*) as discharges
from [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_study_pop_deriv_start
	where 
	1=1
    and der_age_at_cds_activity_date >= 65
	and Der_National_POD_Code <> 'RADAY'
	and Der_National_POD_Code <> 'RANIGHT'
	and discharge_method <> '4'
	and discharge_destination = 19


union all

select '4_exclLoS60' as stage, COUNT(*) as discharges
from [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_study_pop_deriv_start
	where 
	1=1
    and der_age_at_cds_activity_date >= 65
	and Der_National_POD_Code <> 'RADAY'
	and Der_National_POD_Code <> 'RANIGHT'
	and discharge_method <> '4'
	and discharge_destination = 19
	and der_spell_LoS <= 60



union all

select '5_exclMissingNHSNo' as stage, COUNT(*) as discharges
from [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_study_pop_deriv_start
	where 
	1=1
    and der_age_at_cds_activity_date >= 65
	and Der_National_POD_Code <> 'RADAY'
	and Der_National_POD_Code <> 'RANIGHT'
	and discharge_method <> '4'
	and discharge_destination = 19
	and der_spell_LoS <= 60
	and der_pseudo_nhs_number is not null
	and discharge_date  is not null
-- note this should be 968,321 - but the data has obviousy moved on


union all
select '5_exclMissingNHSNo_reconciled' as stage, COUNT(*) as discharges
from [NHSE_Sandbox_StrategyUnit].dbo.sw__CommRehabEqu_ipDischarges



union all

select '6_exclMissingBadField' as stage, COUNT(*) as discharges
from [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_study_pop_deriv_start
	where 
	1=1
    and der_age_at_cds_activity_date >= 65
	and Der_National_POD_Code <> 'RADAY'
	and Der_National_POD_Code <> 'RANIGHT'
	and discharge_method <> '4'
	and discharge_destination = 19
	and der_spell_LoS <= 60
	and der_pseudo_nhs_number is not null
	and discharge_date  is not null
	and der_age_at_cds_activity_date <= 110
	and sex in ('1', '2') 
	and imd2019Dec is not null
	and Der_Admit_Treatment_Function_Code is not null
	and Der_National_POD_Code <> 'APCUNK'

union all

select '6_exclMissingBadField_reconciled' as stage, COUNT(*) as discharges
from [NHSE_Sandbox_StrategyUnit].dbo.sw__CommRehabEqu_ipDischarges
	where 
	1=1
	and der_age_at_cds_activity_date <= 110
	and sex in ('1', '2') 
	and imd2019Dec is not null
	and Der_Admit_Treatment_Function_Code is not null
	and Der_National_POD_Code <> 'APCUNK'


union all

select '7_exclDorset' as stage, COUNT(*) as discharges
from [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_study_pop_deriv_start
	where 
	1=1
    and der_age_at_cds_activity_date >= 65
	and Der_National_POD_Code <> 'RADAY'
	and Der_National_POD_Code <> 'RANIGHT'
	and discharge_method <> '4'
	and discharge_destination = 19
	and der_spell_LoS <= 60
	and der_pseudo_nhs_number is not null
	and discharge_date  is not null
	and der_age_at_cds_activity_date <= 110
	and sex in ('1', '2') 
	and imd2019Dec is not null
	and Der_Admit_Treatment_Function_Code is not null
	and Der_National_POD_Code <> 'APCUNK'
	and STP21CD <> 'E54000041'

union all

select '7_exclDorset_reconciled' as stage, COUNT(*) as discharges
from [NHSE_Sandbox_StrategyUnit].dbo.sw__CommRehabEqu_ipDischarges
	where 
	1=1
	and der_age_at_cds_activity_date <= 110
	and sex in ('1', '2') 
	and imd2019Dec is not null
	and Der_Admit_Treatment_Function_Code is not null
	and Der_National_POD_Code <> 'APCUNK'
	and STP21CD <> 'E54000041'


order by stage
