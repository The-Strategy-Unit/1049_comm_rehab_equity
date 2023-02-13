select [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_provMonthSubmission.OrgID_Provider, 
[NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_provMonthSubmission.rp_startDate,
dominantSubmission,
dominantUniqueSubmissionID, 
count(*) as careContacts,
COUNT(distinct resubmission201.CareContactID) as distinctResubmissionCareContactIDs,
COUNT(distinct primarySubmission201.CareContactID) as distinctPrimarySubmissionCareContactIDs

from [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_provMonthSubmission --note: this is just a cross join of month and provider


left outer join (select distinct [NHSE_PSDM_COVID19].[com].[PublishCYP201CareContact].OrgID_Provider,  
				[NHSE_PSDM_COVID19].[com].[PublishCYP201CareContact].CareContactID, 
				[NHSE_PSDM_COVID19].[com].[PublishCYP201CareContact].Person_ID,  
				[NHSE_PSDM_COVID19].[com].[PublishCYP201CareContact].UniqueSubmissionID, 
				[NHSE_PSDM_COVID19].[com].[PublishCYP201CareContact].[Contact_Date], 
				[NHSE_PSDM_COVID19].[com].[PublishCYP201CareContact].[Contact_Time], 
				[NHSE_PSDM_COVID19].[com].[PublishCYP201CareContact].AttendOrNot,
				[NHSE_PSDM_COVID19].[com].[PublishCYP201CareContact].Consultation_MediumUsed

				from 	(select 
							OrgID_Provider,
							CareContactID, 
							MAX(RecordNumber) as RecordNumber

							from [NHSE_PSDM_COVID19].[com].[PublishCYP201CareContact]

							group by 
							OrgID_Provider,
							CareContactID) uniqueContact

				inner join [NHSE_PSDM_COVID19].[com].[PublishCYP201CareContact]
				on [NHSE_PSDM_COVID19].[com].[PublishCYP201CareContact].OrgID_Provider = uniqueContact.OrgID_Provider
				and [NHSE_PSDM_COVID19].[com].[PublishCYP201CareContact].CareContactID = uniqueContact.CareContactID
				and [NHSE_PSDM_COVID19].[com].[PublishCYP201CareContact].RecordNumber = uniqueContact.RecordNumber

				where [NHSE_PSDM_COVID19].[com].[PublishCYP201CareContact].[Contact_Date] >= '2022-04-01'
				and [NHSE_PSDM_COVID19].[com].[PublishCYP201CareContact].[Contact_Date] <= '2022-08-31') resubmission201

on [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_provMonthSubmission.dominantSubmission = 'resubmission'
and datepart(month, [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_provMonthSubmission.RP_StartDate) = datepart(month, resubmission201.Contact_Date)
and datepart(year, [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_provMonthSubmission.RP_StartDate) = datepart(year, resubmission201.Contact_Date)
and [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_provMonthSubmission.OrgID_Provider = resubmission201.OrgID_Provider
and [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_provMonthSubmission.dominantUniqueSubmissionID = resubmission201.UniqueSubmissionID

left outer join (select distinct  [NHSE_PSDM_COVID19].[comP].[PublishCYP201CareContact].OrgID_Provider, 
				[NHSE_PSDM_COVID19].[comP].[PublishCYP201CareContact].CareContactID, 
				[NHSE_PSDM_COVID19].[comP].[PublishCYP201CareContact].Person_ID,  
				[NHSE_PSDM_COVID19].[comP].[PublishCYP201CareContact].UniqueSubmissionID, 
				[NHSE_PSDM_COVID19].[comP].[PublishCYP201CareContact].[Contact_Date], 
				[NHSE_PSDM_COVID19].[comP].[PublishCYP201CareContact].[Contact_Time], 
				[NHSE_PSDM_COVID19].[comP].[PublishCYP201CareContact].AttendOrNot,
				[NHSE_PSDM_COVID19].[comP].[PublishCYP201CareContact].Consultation_MediumUsed

				from 	(select 
							OrgID_Provider,
							CareContactID, 
							MAX(RecordNumber) as RecordNumber

							from [NHSE_PSDM_COVID19].[comP].[PublishCYP201CareContact]

							group by 
							OrgID_Provider,
							CareContactID) uniqueContact

				inner join [NHSE_PSDM_COVID19].[comP].[PublishCYP201CareContact]
				on [NHSE_PSDM_COVID19].[comP].[PublishCYP201CareContact].OrgID_Provider = uniqueContact.OrgID_Provider
				and [NHSE_PSDM_COVID19].[comP].[PublishCYP201CareContact].CareContactID = uniqueContact.CareContactID
				and [NHSE_PSDM_COVID19].[comP].[PublishCYP201CareContact].RecordNumber = uniqueContact.RecordNumber

				where [NHSE_PSDM_COVID19].[comP].[PublishCYP201CareContact].[Contact_Date] >= '2022-04-01'
				and [NHSE_PSDM_COVID19].[comP].[PublishCYP201CareContact].[Contact_Date] <= '2022-08-31') primarySubmission201

on [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_provMonthSubmission.dominantSubmission = 'primarySubmission'
and datepart(month, [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_provMonthSubmission.RP_StartDate) = datepart(month, primarySubmission201.Contact_Date)
and datepart(year, [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_provMonthSubmission.RP_StartDate) = datepart(year, primarySubmission201.Contact_Date)
and [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_provMonthSubmission.OrgID_Provider = primarySubmission201.OrgID_Provider
and [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_provMonthSubmission.dominantUniqueSubmissionID = primarySubmission201.UniqueSubmissionID


where 
[NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_provMonthSubmission.rp_startDate >= '2022-04-01'
and [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_provMonthSubmission.rp_startDate <= '2022-08-01'

group by 
[NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_provMonthSubmission.OrgID_Provider, 
[NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_provMonthSubmission.rp_startDate,
dominantSubmission,
dominantUniqueSubmissionID