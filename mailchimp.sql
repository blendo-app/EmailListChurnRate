select users.blendo_id ,
users.blendo_imported_at ,
users.email_address ,
users.email_client ,
users.email_type ,
users.id ,
users.ip_opt ,
users.ip_signup ,
users.language ,
users.last_changed ,
users.list_id ,
users.location_country_code ,
users.location_dstoff ,
users.location_gmtoff ,
users.location_latitude ,
users.location_longitude ,
users.location_timezone ,
users.member_rating ,
users.stats_avg_click_rate ,
users.stats_avg_open_rate ,
users.status ,
users.timestamp_opt ,
users.timestamp_signup ,
users.unique_email_id ,
users.vip ,
users.unsubscribe_reason,
activity.TotalOpens,
activity.TotalClicks,
activity.TotalBounces
from mailchimp_list_members as users
left join (
	select email_address, list_id, 
  	count(case when mailchimp_report_email_activity.action='open' then 1 else NULL end) as TotalOpens,
	count(case when mailchimp_report_email_activity.action='click' then 1 else NULL end) as TotalClicks,
	count (case when mailchimp_report_email_activity.action='bounce' then 1 else NULL end) as TotalBounces
	from mailchimp_report_email_activity
	group by email_address, list_id) as activity
on users.email_address=activity.email_address and users.list_id=activity.list_id