DROP TABLE IF EXISTS yellow_monthly_reports CASCADE;
DROP TABLE IF EXISTS fhv_weekly_reports CASCADE;

CREATE TABLE yellow_monthly_reports (
  month date,
  trips_per_day integer,
  farebox_per_day integer,
  unique_drivers integer,
  unique_medallions integer,
  medallions_per_day integer,
  avg_days_medallions_on_road numeric,
  avg_hours_per_day_per_medallion numeric,
  avg_days_drivers_on_road numeric,
  avg_hours_per_day_per_driver numeric,
  avg_minutes_per_trip numeric,
  percent_trips_paid_with_credit_card numeric
);

CREATE UNIQUE INDEX index_yellow_monthly_uniq ON yellow_monthly_reports (month);

CREATE TABLE fhv_weekly_reports (
  base_number varchar,
  wave_number integer,
  base_name varchar,
  dba varchar,
  year integer,
  week_number integer,
  pickup_start_date date,
  pickup_end_date date,
  total_dispatched_trips integer,
  unique_dispatched_vehicles integer,
  dba_category varchar
);

CREATE UNIQUE INDEX index_fhv_weekly_uniq ON fhv_weekly_reports (base_number, year, week_number);
CREATE INDEX index_fhv_on_category ON fhv_weekly_reports (dba_category, pickup_end_date);

CREATE VIEW fhv_weekly_reports_intermediate_view AS
SELECT
  *,
  LAG(unique_dispatched_vehicles, 1) OVER (PARTITION BY base_number ORDER BY year, week_number) AS prev_uniq_vehicles,
  LEAD(unique_dispatched_vehicles, 1) OVER (PARTITION BY base_number ORDER BY year, week_number) AS next_uniq_vehicles
FROM fhv_weekly_reports;

CREATE VIEW fhv_weekly_reports_view AS
SELECT
  *,
  (unique_dispatched_vehicles > 5
    AND unique_dispatched_vehicles::numeric / prev_uniq_vehicles > 1.4
    AND unique_dispatched_vehicles::numeric / next_uniq_vehicles > 1.4) AS unreliable_vehicles_count
FROM fhv_weekly_reports_intermediate_view;
