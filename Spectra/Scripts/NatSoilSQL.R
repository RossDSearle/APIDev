
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



INSERT INTO projects(agency_code, proj_code, proj_name) VALUES ('601', 'SpecDemo', 'This is a test project for development purpose in the Spectra Infrastructure Project' );


select * from ARCHIVE_SAMPLES where proj_code = 'SpecDemo';











SELECT        SpectraMeta.SpectraID, SITES.agency_code, SITES.proj_code, SITES.s_id, OBSERVATIONS.o_id, HORIZONS.h_no, SAMPLES.samp_no, SpectraMeta.DataPath, SpectraMeta.Type, SpectraMeta.Username, 
SpectraMeta.SubmitTime, SpectraMeta.Lattitude, SpectraMeta.Longitude, SpectraMeta.UpperDepth, SpectraMeta.LowerDepth, SpectraMeta.OriginalName
FROM            SITES INNER JOIN
OBSERVATIONS ON SITES.agency_code = OBSERVATIONS.agency_code AND SITES.proj_code = OBSERVATIONS.proj_code AND SITES.s_id = OBSERVATIONS.s_id INNER JOIN
HORIZONS ON OBSERVATIONS.agency_code = HORIZONS.agency_code AND OBSERVATIONS.proj_code = HORIZONS.proj_code AND OBSERVATIONS.s_id = HORIZONS.s_id AND 
OBSERVATIONS.o_id = HORIZONS.o_id INNER JOIN
SAMPLES ON HORIZONS.agency_code = SAMPLES.agency_code AND HORIZONS.proj_code = SAMPLES.proj_code AND HORIZONS.s_id = SAMPLES.s_id AND HORIZONS.o_id = SAMPLES.o_id AND 
HORIZONS.h_no = SAMPLES.h_no INNER JOIN
ARCHIVE_SAMPLES ON SAMPLES.agency_code = ARCHIVE_SAMPLES.agency_code AND SAMPLES.proj_code = ARCHIVE_SAMPLES.proj_code AND SAMPLES.s_id = ARCHIVE_SAMPLES.s_id AND 
SAMPLES.o_id = ARCHIVE_SAMPLES.o_id AND SAMPLES.h_no = ARCHIVE_SAMPLES.h_no AND SAMPLES.samp_no = ARCHIVE_SAMPLES.samp_no INNER JOIN
SpectraMeta ON ARCHIVE_SAMPLES.spec_id = SpectraMeta.SpectraID 

WHERE SITES.proj_code = 'SpecDemo' and SpectraMeta.Username = 'Ross'
