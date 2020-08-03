
select * from SITES where proj_code = 'spectest';
select * from OBSERVATIONS where proj_code = 'spectest';
select * from HORIZONS where proj_code = 'spectest';
select * from SAMPLES where proj_code = 'spectest';
select * from ARCHIVE_SAMPLES where proj_code = 'spectest';

select * from SAMPLES where agency_code='601' and proj_code='spectest' and s_id='1' and o_id='1' and h_no=1 and samp_no=1;
delete from SAMPLES where agency_code='601' and proj_code='spectest' and s_id='1' and o_id='1' and h_no=1 and samp_no=1;

select * from ARCHIVE_SAMPLES where agency_code='601' and proj_code='spectest' and s_id='1' and o_id='1' and h_no=1 and samp_no=1;

select max(CAST(s_id AS int)) from sites where agency_code='601' and proj_code='spectest';

select max(CAST(s_id AS int)) from sites where agency_code='601' and proj_code='spectest';





INSERT INTO projects(agency_code, proj_code, proj_name) VALUES ('601', 'spectest', 'This is a test proj' );

INSERT INTO sites(agency_code, proj_code, s_id) VALUES ('601', 'spectest', '2' );

INSERT INTO OBSERVATIONS(agency_code, proj_code, s_id, o_id) VALUES ('601', 'spectest', '1', '1' );

INSERT INTO HORIZONS(agency_code, proj_code, s_id, o_id, h_no) VALUES ('601', 'spectest', '1', '1', 1 );
INSERT INTO SAMPLES(agency_code, proj_code, s_id, o_id, h_no, samp_no, samp_upper_depth, samp_lower_depth) VALUES ('601', 'spectest', '1', '1', 1, 1, 0, 0.2 );

INSERT INTO ARCHIVE_SAMPLES(agency_code, proj_code, s_id, o_id, h_no, samp_no, jar_no, samp_type, [location], [weight],[>2mm], spec_id, subsample_date, subsample_tray)
VALUES ('601', 'spectest', '1','1', 1, 1, 1, 'xx', 'here', 123, 1, 12345678,'',''  );


