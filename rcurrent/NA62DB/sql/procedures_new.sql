CREATE OR REPLACE PROCEDURE  "CREATEPROVA"
(
  Detector_prefix IN VARCHAR2,
  SQLCommandOut OUT VARCHAR2
) IS
TableName VARCHAR2(255) := '';
SQLCommand VARCHAR2(4000) := '';

BEGIN
TableName:=Detector_prefix || '_tmpDataVal';

 BEGIN
  execute immediate 'DROP TABLE '||TableName;
  EXCEPTION
  WHEN OTHERS THEN
  NULL;
 END;

SQLCommand:='
CREATE TABLE '||TableName||'
(
ID NUMBER(32),
SUBSYSTEMTYPENAME VARCHAR2(255),
SUBSYSTEMID NUMBER(10,0),
ATTRIBUTENAME VARCHAR2(255),
VALUE BLOB,
TimeOfValAdding TIMESTAMP (0),
ValueValidityStart TIMESTAMP (0),
ValueValidityFinish TIMESTAMP (0),
STORAGEDATATYPE VARCHAR2(255)
)';
execute immediate SQLCommand;

SQLCommand:='CREATE OR REPLACE FORCE VIEW "'||Detector_prefix||'_AttrDataTypeView" ("StorageDataType", "AttrName", "SubSystemTypeName", "RelationshipValidityStart", "RelationshipValidityFinish") AS
  ( SELECT "'||Detector_prefix||'_DataType"."StorageDataType" AS
  "StorageDataType", "'||Detector_prefix||'_Attributies"."Name" AS
  "AttrName", "'||Detector_prefix||'_RelationshipValidity"."SubSystemTypeName" AS
  "SubSystemTypeName", "'||Detector_prefix||'_RelationshipValidity"."RelationshipValidityStart"
  AS "RelationshipValidityStart", "'||Detector_prefix||'_RelationshipValidity"."RelationshipValidityFinish"
  AS "RelationshipValidityFinish"
  FROM NA62_CONDDB_ADMIN."'||Detector_prefix||'_DataType" "'||Detector_prefix||'_DataType",
  NA62_CONDDB_ADMIN."'||Detector_prefix||'_Attributies" "'||Detector_prefix||'_Attributies",
  NA62_CONDDB_ADMIN."'||Detector_prefix||'_RelationshipValidity"
  "'||Detector_prefix||'_RelationshipValidity" WHERE "'||Detector_prefix||'_DataType"."DataTypeName" = "'||Detector_prefix||'_Attributies"."DataTypeName"
  AND "'||Detector_prefix||'_RelationshipValidity"."AttributeID" = "'||Detector_prefix||'_Attributies"."AttributeID" )';
execute immediate SQLCommand;

SQLCommandOut:='CREATE OR REPLACE FORCE VIEW "'||Detector_prefix||'_CONSOLIDATEVIEW" ("ID", "SUBSYSTEMID", "ATTRIBUTENAME", "STORAGEDATATYPE", "SUBSYSTEMTYPENAME", "TIMEOFVALADDING", "VALUE", "VALUEVALIDITYSTART", "VALUEVALIDITYFINISH") AS
  SELECT
    CAST(ROWNUM AS NUMBER(9)) AS "ID",
      CAST("'||Detector_prefix||'_Values"."SubSystemID" AS NUMBER(9)) AS "SUBSYSTEMID",
        "'||Detector_prefix||'_Attributies"."Name" AS "ATTRIBUTENAME",
	  "'||Detector_prefix||'_DataType"."StorageDataType" AS "STORAGEDATATYPE",
	    "'||Detector_prefix||'_RelationshipValidity"."SubSystemTypeName" AS "SUBSYSTEMTYPENAME",
	      TO_CHAR(CAST ("'||Detector_prefix||'_Values"."TimeOfValAdding" AS TIMESTAMP(6)), ''YYYYMMDDHH24MISSFF'') AS "TIMEOFVALADDING",
	        "'||Detector_prefix||'_Values"."ValueContent" AS "VALUE",

		   CASE WHEN  "'||Detector_prefix||'_Values"."ValueValidityStart" >= "'||Detector_prefix||'_RelationshipValidity"."RelationshipValidityStart" THEN TO_CHAR(CAST("'||Detector_prefix||'_Values"."ValueValidityStart" AS TIMESTAMP(6)), ''YYYYMMDDHH24MISSFF'')

		   WHEN  "'||Detector_prefix||'_Values"."ValueValidityStart" < "'||Detector_prefix||'_RelationshipValidity"."RelationshipValidityStart" THEN TO_CHAR(CAST("'||Detector_prefix||'_RelationshipValidity"."RelationshipValidityStart" AS TIMESTAMP(6)), ''YYYYMMDDHH24MISSFF'')

        END AS "VALUEVALIDITYSTART",

	   CASE WHEN  "'||Detector_prefix||'_Values"."ValueValidityFinish" <= "'||Detector_prefix||'_RelationshipValidity"."RelationshipValidityFinish" THEN TO_CHAR(CAST("'||Detector_prefix||'_Values"."ValueValidityFinish" AS TIMESTAMP(6)), ''YYYYMMDDHH24MISSFF'')

		     WHEN  "'||Detector_prefix||'_Values"."ValueValidityFinish" > "'||Detector_prefix||'_RelationshipValidity"."RelationshipValidityFinish" THEN TO_CHAR(CAST("'||Detector_prefix||'_RelationshipValidity"."RelationshipValidityFinish" AS TIMESTAMP(6)), ''YYYYMMDDHH24MISSFF'')

	     END AS "VALUEVALIDITYFINISH"

   FROM "'||Detector_prefix||'_RelationshipValidity", "'||Detector_prefix||'_Attributies", "'||Detector_prefix||'_DataType", "'||Detector_prefix||'_Values", "'||Detector_prefix||'_DetectorSubSystem"
   WHERE
     ("'||Detector_prefix||'_Attributies"."AttributeID" = "'||Detector_prefix||'_RelationshipValidity"."AttributeID") AND
     ("'||Detector_prefix||'_Attributies"."DataTypeName" = "'||Detector_prefix||'_DataType"."DataTypeName") AND
     ("'||Detector_prefix||'_Values"."AttributeID" = "'||Detector_prefix||'_RelationshipValidity"."AttributeID")  AND
     ("'||Detector_prefix||'_Values"."SubSystemID" = "'||Detector_prefix||'_DetectorSubSystem"."SubSystemID")  AND
     ("'||Detector_prefix||'_RelationshipValidity"."SubSystemTypeName" = "'||Detector_prefix||'_DetectorSubSystem"."SubSystemTypeName")';
    
/*
execute immediate SQLCommandOut;
*/


SQLCommand:='CREATE OR REPLACE FORCE VIEW "'||Detector_prefix||'_CORALAVLOIDERSST0" ("SSTCOUNT") AS
  SELECT
   CAST(COUNT(*) AS NUMBER(9)) AS "SSTCOUNT"
FROM
   "'||Detector_prefix||'_SubSystemType"';
execute immediate SQLCommand;


SQLCommand:='CREATE OR REPLACE FORCE VIEW "'||Detector_prefix||'_CORALAVLOIDERSSTC1" ("SUBSYSTEMTYPENAME", "SUBSYSTEMCOUNT") AS
  SELECT
  "'||Detector_prefix||'_DetectorSubSystem"."SubSystemTypeName"  AS "SUBSYSTEMTYPENAME",
  CAST (COUNT("'||Detector_prefix||'_DetectorSubSystem"."SubSystemTypeName") AS NUMBER(9)) AS "SUBSYSTEMCOUNT"
FROM
   "'||Detector_prefix||'_DetectorSubSystem"
GROUP BY  "'||Detector_prefix||'_DetectorSubSystem"."SubSystemTypeName"';
execute immediate SQLCommand;


SQLCommand:='CREATE OR REPLACE FORCE VIEW "'||Detector_prefix||'_CORALAVOIDERTCS" ("TOTALSUBSYSTEM") AS
  SELECT
    CAST (COUNT ("'||Detector_prefix||'_DetectorSubSystem"."SubSystemID") AS NUMBER(9))AS "TOTALSUBSYSTEM"
FROM "'||Detector_prefix||'_DetectorSubSystem"';
execute immediate SQLCommand;



END CREATEPROVA;
/

