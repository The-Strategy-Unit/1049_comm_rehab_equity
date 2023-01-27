use [NHSE_Sandbox_StrategyUnit]

 --drop table [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_provMonth
 --drop table [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_provMonthSubmission
 --drop table [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_uniqueContactsResub
 --drop table [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_uniquePatientsResub
 --drop table [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_uniqueContactsPrimSub
 --drop table [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_uniquePatientsPrimSub
 --drop table [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_CommContacts
 --drop table [NHSE_Sandbox_StrategyUnit].dbo.sw__CommRehabEqu_ipDischarges
 --drop table [NHSE_Sandbox_StrategyUnit].dbo.sw__CommRehabEqu_ipDischarges_priorCommContacts
 --drop table [NHSE_Sandbox_StrategyUnit].dbo.sw__CommRehabEqu_ipDischarges_priorPostCommContacts
 --drop table [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_CommContacts_lsoa_counts

-- create table for each month and provider
select OrgID_Provider, rp_startDate
into [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_provMonth
from [NHSE_PSDM_COVID19].[com].[PublishCYP000Header]
union
select OrgID_Provider, rp_startDate
from [NHSE_PSDM_COVID19].[comP].[PublishCYP000Header]



-- add columns to indicate whether to take prinary or resubmission and if so which one
select 
dbo.sw_CommRehabEqu_provMonth.OrgID_Provider, 
dbo.sw_CommRehabEqu_provMonth.rp_startDate,
case
	when resubmissions.[UniqueSubmissionID] is not null then 'resubmission' 
	when primarySubmissions.[UniqueSubmissionID] is not null then 'primarySubmission' 
	else NULL
	end as dominantSubmission,
case
	when resubmissions.[UniqueSubmissionID] is not null then resubmissions.[UniqueSubmissionID]
	when primarySubmissions.[UniqueSubmissionID] is not null then primarySubmissions.[UniqueSubmissionID]
	else NULL
	end as dominantUniqueSubmissionID

into [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_provMonthSubmission

from [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_provMonth

left outer join (select OrgID_Provider, rp_startDate, max([UniqueSubmissionID]) as [UniqueSubmissionID]
					from [NHSE_PSDM_COVID19].[com].[PublishCYP000Header]
					group by OrgID_Provider, rp_startDate) resubmissions
on dbo.sw_CommRehabEqu_provMonth.OrgID_Provider = resubmissions.OrgID_Provider
and dbo.sw_CommRehabEqu_provMonth.rp_startDate = resubmissions.rp_startDate

left outer join (select OrgID_Provider, rp_startDate, max([UniqueSubmissionID]) as [UniqueSubmissionID]
					from [NHSE_PSDM_COVID19].[comP].[PublishCYP000Header]
					group by OrgID_Provider, rp_startDate) primarySubmissions
on dbo.sw_CommRehabEqu_provMonth.OrgID_Provider = primarySubmissions.OrgID_Provider
and dbo.sw_CommRehabEqu_provMonth.rp_startDate = primarySubmissions.rp_startDate



-- create table of unique contacts resubmission

select 
OrgID_Provider,
CareContactID, 
MAX(RecordNumber) as RecordNumber

into [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_uniqueContactsResub

from [NHSE_PSDM_COVID19].[com].[PublishCYP201CareContact]

group by 
OrgID_Provider,
CareContactID


-- create table of unique patients resubmission

select 
OrgID_Provider, 
Person_ID, 
MAX(RecordNumber) as RecordNumber
into [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_uniquePatientsResub
from [NHSE_PSDM_COVID19].[com].[PublishCYP001MPI]
where pseudo_nhs_number_ncdr is not null
group by OrgID_Provider, Person_ID



-- create table of all comm services contacts between 1 March and 31 August 2022
select distinct
	resubContact.*,
	patient.pseudo_nhs_number_ncdr,
	patient.LSOA

	into [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_CommContacts

	from
		(select distinct [NHSE_PSDM_COVID19].[com].[PublishCYP201CareContact].OrgID_Provider,  
						[NHSE_PSDM_COVID19].[com].[PublishCYP201CareContact].CareContactID, 
						[NHSE_PSDM_COVID19].[com].[PublishCYP201CareContact].Person_ID,  
						[NHSE_PSDM_COVID19].[com].[PublishCYP201CareContact].UniqueSubmissionID, 
						[NHSE_PSDM_COVID19].[com].[PublishCYP201CareContact].[Contact_Date], 
						[NHSE_PSDM_COVID19].[com].[PublishCYP201CareContact].[Contact_Time], 
						[NHSE_PSDM_COVID19].[com].[PublishCYP201CareContact].AttendOrNot,
						[NHSE_PSDM_COVID19].[com].[PublishCYP201CareContact].Consultation_MediumUsed,
						1 as resub

		from [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_uniqueContactsResub uniqueContact

		inner join [NHSE_PSDM_COVID19].[com].[PublishCYP201CareContact]
		on [NHSE_PSDM_COVID19].[com].[PublishCYP201CareContact].OrgID_Provider = uniqueContact.OrgID_Provider
		and [NHSE_PSDM_COVID19].[com].[PublishCYP201CareContact].CareContactID = uniqueContact.CareContactID
		and [NHSE_PSDM_COVID19].[com].[PublishCYP201CareContact].RecordNumber = uniqueContact.RecordNumber

		where [NHSE_PSDM_COVID19].[com].[PublishCYP201CareContact].[Contact_Date] >= '2022-03-01'
		and [NHSE_PSDM_COVID19].[com].[PublishCYP201CareContact].[Contact_Date] <= '2022-08-31'
		) resubContact

	inner join [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_provMonthSubmission subMonthDominant
	on subMonthDominant.dominantSubmission = 'resubmission'
		and datepart(month, subMonthDominant.RP_StartDate) = datepart(month, resubContact.Contact_Date)
		and datepart(year, subMonthDominant.RP_StartDate) = datepart(year, resubContact.Contact_Date)
		and subMonthDominant.OrgID_Provider = resubContact.OrgID_Provider
		and subMonthDominant.dominantUniqueSubmissionID = resubContact.UniqueSubmissionID
	
	
	left outer join [NHSE_PSDM_COVID19].[com].[PublishCYP001MPI] patient
	on resubContact.OrgID_Provider = patient.OrgID_Provider
	and resubContact.Person_ID = patient.Person_ID


	left outer join [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_uniquePatientsResub uniquePatient

	on patient.OrgID_Provider = uniquePatient.OrgID_Provider
	and patient.Person_ID = uniquePatient.Person_ID
	and patient.RecordNumber = uniquePatient.RecordNumber

	where 
	uniquePatient.RecordNumber is not null



-- create table of unique contacts primary submission

select 
OrgID_Provider,
CareContactID, 
MAX(RecordNumber) as RecordNumber

into [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_uniqueContactsPrimSub

from [NHSE_PSDM_COVID19].[comP].[PublishCYP201CareContact]

group by 
OrgID_Provider,
CareContactID




-- create table of unique patients  primary submission

select 
OrgID_Provider, 
Person_ID, 
MAX(RecordNumber) as RecordNumber
into [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_uniquePatientsPrimSub
from [NHSE_PSDM_COVID19].[comP].[PublishCYP001MPI]
where pseudo_nhs_number_ncdr is not null
group by OrgID_Provider, Person_ID




--add contact from primary submissions
insert into [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_CommContacts

select distinct
	primaryContact.*,
	patient.pseudo_nhs_number_ncdr,
	patient.LSOA

	from

		(select distinct [NHSE_PSDM_COVID19].[comP].[PublishCYP201CareContact].OrgID_Provider, 
						[NHSE_PSDM_COVID19].[comP].[PublishCYP201CareContact].CareContactID, 
						[NHSE_PSDM_COVID19].[comP].[PublishCYP201CareContact].Person_ID,  
						[NHSE_PSDM_COVID19].[comP].[PublishCYP201CareContact].UniqueSubmissionID, 
						[NHSE_PSDM_COVID19].[comP].[PublishCYP201CareContact].[Contact_Date], 
						[NHSE_PSDM_COVID19].[comP].[PublishCYP201CareContact].[Contact_Time], 
						[NHSE_PSDM_COVID19].[comP].[PublishCYP201CareContact].AttendOrNot,
						[NHSE_PSDM_COVID19].[comP].[PublishCYP201CareContact].Consultation_MediumUsed,
						0 as resub

		from 	[NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_uniqueContactsPrimSub uniqueContact

		inner join [NHSE_PSDM_COVID19].[comP].[PublishCYP201CareContact]
		on [NHSE_PSDM_COVID19].[comP].[PublishCYP201CareContact].OrgID_Provider = uniqueContact.OrgID_Provider
		and [NHSE_PSDM_COVID19].[comP].[PublishCYP201CareContact].CareContactID = uniqueContact.CareContactID
		and [NHSE_PSDM_COVID19].[comP].[PublishCYP201CareContact].RecordNumber = uniqueContact.RecordNumber

		where [NHSE_PSDM_COVID19].[comP].[PublishCYP201CareContact].[Contact_Date] >= '2022-03-01'
		and [NHSE_PSDM_COVID19].[comP].[PublishCYP201CareContact].[Contact_Date] <= '2022-08-31'
		) primaryContact


	inner join [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_provMonthSubmission subMonthDominant
	on subMonthDominant.dominantSubmission = 'primarySubmission'
		and datepart(month, subMonthDominant.RP_StartDate) = datepart(month, primaryContact.Contact_Date)
		and datepart(year, subMonthDominant.RP_StartDate) = datepart(year, primaryContact.Contact_Date)
		and subMonthDominant.OrgID_Provider = primaryContact.OrgID_Provider
		and subMonthDominant.dominantUniqueSubmissionID = primaryContact.UniqueSubmissionID

	left outer join [NHSE_PSDM_COVID19].[comP].[PublishCYP001MPI] patient
	on primaryContact.OrgID_Provider = patient.OrgID_Provider
	and primaryContact.Person_ID = patient.Person_ID


	left outer join [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_uniquePatientsPrimSub uniquePatient

	on patient.OrgID_Provider = uniquePatient.OrgID_Provider
	and patient.Person_ID = uniquePatient.Person_ID
	and patient.RecordNumber = uniquePatient.RecordNumber

	where 
	uniquePatient.RecordNumber is not null







-- create table of ip discharges between 1 June 2022 and 31 July 2022
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
admission_date, discharge_date

into [NHSE_Sandbox_StrategyUnit].dbo.sw__CommRehabEqu_ipDischarges

from NHSE_SUSPlus_Live.[dbo].[tbl_Data_SEM_APCS]

left outer join [NHSE_Sandbox_Spec_Neurology].[dbo].[RefIMD2019]
on NHSE_SUSPlus_Live.[dbo].[tbl_Data_SEM_APCS].Der_Postcode_LSOA_2011_Code = [NHSE_Sandbox_Spec_Neurology].[dbo].[RefIMD2019].[LSOAcode2011]

left outer join [NHSE_Sandbox_StrategyUnit].[dbo].[LSOA(2011)_to_CCG_to_STP_(April_2021)]
on NHSE_SUSPlus_Live.[dbo].[tbl_Data_SEM_APCS].Der_Postcode_LSOA_2011_Code = [NHSE_Sandbox_StrategyUnit].[dbo].[LSOA(2011)_to_CCG_to_STP_(April_2021)].[LSOA11CD]

where der_age_at_cds_activity_date >= 65
and discharge_date  is not null
and discharge_date >= '2022-06-01 00:00:00.000'
and discharge_date <= '2022-07-31 00:00:00.000'
and discharge_method <> '4'
and discharge_destination = 19
and der_pseudo_nhs_number is not null
and Der_National_POD_Code <> 'RADAY'
and Der_National_POD_Code <> 'RANIGHT'
and der_spell_LoS <= 60

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




--select top 10 * from [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_CommContacts
--select top 10 * from [NHSE_Sandbox_StrategyUnit].dbo.sw__CommRehabEqu_ipDischarges


-- add prior discharge comm contacts (in 30 days prior to admission)
select 
der_pseudo_nhs_number,
Der_Age_at_CDS_Activity_Date,
sex,
ethnic_group,
imd2019Dec,
[NHSE_Sandbox_StrategyUnit].dbo.sw__CommRehabEqu_ipDischarges.[STP21CD],
[NHSE_Sandbox_StrategyUnit].dbo.sw__CommRehabEqu_ipDischarges.[STP21NM],
der_spell_LoS,
der_diagnosis_all,
der_diagnosis_count,
der_procedure_count,
der_admit_treatment_function_code,
der_national_pod_code,
admission_date, discharge_date,
SUM(case when 
	[NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_CommContacts.AttendOrNot in ('5', '6') 
	and [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_CommContacts.Consultation_MediumUsed = '01' 
	then 1 else 0 end) as priorCommContactsAttF2F,
SUM(case when 
	[NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_CommContacts.AttendOrNot in ('5', '6')
	then 1 else 0 end) as priorCommContactsAtt,
SUM(case when 
	[NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_CommContacts.pseudo_nhs_number_ncdr is not null
	then 1 else 0 end) as priorCommContactsApp

into [NHSE_Sandbox_StrategyUnit].dbo.sw__CommRehabEqu_ipDischarges_priorCommContacts

from [NHSE_Sandbox_StrategyUnit].dbo.sw__CommRehabEqu_ipDischarges

left outer join [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_CommContacts
on [NHSE_Sandbox_StrategyUnit].dbo.sw__CommRehabEqu_ipDischarges.der_pseudo_nhs_number = 
	[NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_CommContacts.pseudo_nhs_number_ncdr
and datediff(dd, 
			[NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_CommContacts.contact_date,
			[NHSE_Sandbox_StrategyUnit].dbo.sw__CommRehabEqu_ipDischarges.admission_date) >= 0
and datediff(dd, 
			[NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_CommContacts.contact_date,
			[NHSE_Sandbox_StrategyUnit].dbo.sw__CommRehabEqu_ipDischarges.admission_date) <= 30

group by 
der_pseudo_nhs_number,
Der_Age_at_CDS_Activity_Date,
sex,
ethnic_group,
imd2019Dec,
[NHSE_Sandbox_StrategyUnit].dbo.sw__CommRehabEqu_ipDischarges.[STP21CD],
[NHSE_Sandbox_StrategyUnit].dbo.sw__CommRehabEqu_ipDischarges.[STP21NM],
der_spell_LoS,
der_diagnosis_all,
der_diagnosis_count,
der_procedure_count,
der_admit_treatment_function_code,
der_national_pod_code,
admission_date, discharge_date




-- add post discharge comm contacts (in 30 days of dischrage)
select 
der_pseudo_nhs_number,
Der_Age_at_CDS_Activity_Date,
sex,
ethnic_group,
imd2019Dec,
[NHSE_Sandbox_StrategyUnit].dbo.sw__CommRehabEqu_ipDischarges_priorCommContacts.[STP21CD],
[NHSE_Sandbox_StrategyUnit].dbo.sw__CommRehabEqu_ipDischarges_priorCommContacts.[STP21NM],
der_spell_LoS,
der_diagnosis_all,
der_diagnosis_count,
der_procedure_count,
der_admit_treatment_function_code,
der_national_pod_code,
admission_date, discharge_date,
priorCommContactsAttF2F,
priorCommContactsAtt,
priorCommContactsApp,
SUM(case when 
	[NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_CommContacts.AttendOrNot in ('5', '6') 
	and [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_CommContacts.Consultation_MediumUsed = '01' 
	then 1 else 0 end) as postCommContactsAttF2F,
SUM(case when 
	[NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_CommContacts.AttendOrNot in ('5', '6')
	then 1 else 0 end) as postCommContactsAtt,
SUM(case when 
	[NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_CommContacts.pseudo_nhs_number_ncdr is not null
	then 1 else 0 end)  as postCommContactsApp

into [NHSE_Sandbox_StrategyUnit].dbo.sw__CommRehabEqu_ipDischarges_priorPostCommContacts

from [NHSE_Sandbox_StrategyUnit].dbo.sw__CommRehabEqu_ipDischarges_priorCommContacts

left outer join [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_CommContacts
on [NHSE_Sandbox_StrategyUnit].dbo.sw__CommRehabEqu_ipDischarges_priorCommContacts.der_pseudo_nhs_number = 
	[NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_CommContacts.pseudo_nhs_number_ncdr
and([NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_CommContacts.AttendOrNot in ('5', '6') 
		or [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_CommContacts.AttendOrNot is null)
and [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_CommContacts.Consultation_MediumUsed = '01'
and datediff(dd,
			[NHSE_Sandbox_StrategyUnit].dbo.sw__CommRehabEqu_ipDischarges_priorCommContacts.discharge_date,
			[NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_CommContacts.contact_date) > 0
and datediff(dd, 
			[NHSE_Sandbox_StrategyUnit].dbo.sw__CommRehabEqu_ipDischarges_priorCommContacts.discharge_date,
			[NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_CommContacts.contact_date) <= 30

group by 
der_pseudo_nhs_number,
Der_Age_at_CDS_Activity_Date,
sex,
ethnic_group,
imd2019Dec,
[NHSE_Sandbox_StrategyUnit].dbo.sw__CommRehabEqu_ipDischarges_priorCommContacts.[STP21CD],
[NHSE_Sandbox_StrategyUnit].dbo.sw__CommRehabEqu_ipDischarges_priorCommContacts.[STP21NM],
der_spell_LoS,
der_diagnosis_all,
der_diagnosis_count,
der_procedure_count,
der_admit_treatment_function_code,
der_national_pod_code,
admission_date, discharge_date,
priorCommContactsAttF2F,
priorCommContactsAtt,
priorCommContactsApp


-- create table of counts of comm contacts by LSOA

select LSOA, 
OrgID_Provider,
count(*) as contactall,
sum(case when AttendOrNot in ('5', '6') then 1 else 0 end) as contactAttended,
sum(case when Consultation_MediumUsed = '01' then 1 else 0 end) as contactF2F,
sum(case when AttendOrNot in ('5', '6') and Consultation_MediumUsed = '01' then 1 else 0 end) as contactAttendedF2F,
sum(case when AttendOrNot is null or AttendOrNot not in ('2', '3', '4', '5', '6', '7') then 1 else 0 end) as contactAttenOrNorInvalid,
sum(case when Consultation_MediumUsed is null or Consultation_MediumUsed not in ('01', '02', '04', '05', '09', '10', '11', '12', '13', '98') then 1 else 0 end) as contactConsMediumInvalid

into [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_CommContacts_lsoa_counts

from [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_CommContacts

group by 
LSOA, 
OrgID_Provider




