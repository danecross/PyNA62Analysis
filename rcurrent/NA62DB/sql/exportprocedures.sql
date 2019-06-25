--------------------------------------------------------
--  File created - Tuesday-December-03-2013   
--------------------------------------------------------
--------------------------------------------------------
--  DDL for Procedure ADDSUBSYSTEM_STORED
--------------------------------------------------------
set define off;

  CREATE OR REPLACE PROCEDURE "NA62_CONDDB_ADMIN"."ADDSUBSYSTEM_STORED" 
(
  Detector_prefix IN VARCHAR2,
  SubSystemTypeName1 IN VARCHAR2,  
  SubSystemCount OUT integer
) IS 
TableName VARCHAR2(255) := '';
SQLCommand VARCHAR2(1000) := '';
BEGIN
    SubSystemCount:=0;
    TableName:=Detector_prefix || '_DetectorSubSystem';
    SQLCommand := 'SELECT count(*) FROM "'||TableName||'"';
    execute immediate SQLCommand into SubSystemCount;
    SQLCommand := 'INSERT INTO "'||TableName||'" ("SubSystemID", "SubSystemTypeName") values(:a,:b)';
    execute immediate SQLCommand using SubSystemCount, SubSystemTypeName1;
    
END ADDSUBSYSTEM_STORED;

/
--------------------------------------------------------
--  DDL for Procedure ADDSUBSYSTEMTYPE_STORED
--------------------------------------------------------
set define off;

  CREATE OR REPLACE PROCEDURE "NA62_CONDDB_ADMIN"."ADDSUBSYSTEMTYPE_STORED" 
(
Detector_prefix IN VARCHAR2,
SubSystemTypeName1 IN VARCHAR2
)
IS 
TableName VARCHAR2(255) := '';
SQLCommand VARCHAR2(1000) := '';
BEGIN
    TableName:=Detector_prefix || '_SubSystemType';
    SQLCommand := 'INSERT INTO "'||TableName||'" ("SubSystemTypeName") values(:a)';
    execute immediate SQLCommand using SubSystemTypeName1;
END ADDSUBSYSTEMTYPE_STORED;

/
--------------------------------------------------------
--  DDL for Procedure CHECKATRRELAIONVALODITYPERIOD
--------------------------------------------------------
set define off;

  CREATE OR REPLACE PROCEDURE "NA62_CONDDB_ADMIN"."CHECKATRRELAIONVALODITYPERIOD" 
(
  Detector_prefix IN VARCHAR2,
  SubSystemTypeName IN VARCHAR2,
  AttributeID IN integer,  
  StartPeriod IN timestamp,
  FinishPeriod IN timestamp,
  IsValid OUT boolean
) IS
TableName VARCHAR2(255) := '';
SQLCommand VARCHAR2(1000) := '';
RowsCount integer;
BEGIN
  IsValid:=false;
  TableName:=Detector_prefix || '_RelationshipValidity';
  SQLCommand := 'SELECT count(*) FROM "'||TableName||'" WHERE (
             ("RelationshipValidityStart" <= :1) AND 
              ("RelationshipValidityFinish" >= :2) AND ("AttributeID"=:3) AND 
              ("SubSystemTypeName"=:4))';
              
  execute immediate SQLCommand into RowsCount using StartPeriod, FinishPeriod, 
              AttributeID, SubSystemTypeName;
  
  IF RowsCount>0 THEN IsValid:=true;
                 ELSE IsValid:=false;
  END IF;
END CHECKATRRELAIONVALODITYPERIOD;

/
--------------------------------------------------------
--  DDL for Procedure CHECKATRRELATIONVALIDITYPERIOD
--------------------------------------------------------
set define off;

  CREATE OR REPLACE PROCEDURE "NA62_CONDDB_ADMIN"."CHECKATRRELATIONVALIDITYPERIOD" 
(
  Detector_prefix IN VARCHAR2,
  SubSystemTypeName IN VARCHAR2,
  AttributeID IN integer,  
  StartPeriod IN timestamp,
  FinishPeriod IN timestamp,
  IsValid OUT boolean
) IS
TableName VARCHAR2(255) := '';
SQLCommand VARCHAR2(1000) := '';
RowsCount integer;
BEGIN
  IsValid:=false;
  TableName:=Detector_prefix || '_RelationshipValidity';
  SQLCommand := 'SELECT count(*) FROM "'||TableName||'" WHERE (
             ("RelationshipValidityStart" <= :1) AND 
              ("RelationshipValidityFinish" >= :2) AND ("AttributeID"=:3) AND 
              ("SubSystemTypeName"=:4))';
              
  execute immediate SQLCommand into RowsCount using StartPeriod, FinishPeriod, 
              AttributeID, SubSystemTypeName;
  
  IF RowsCount>0 THEN IsValid:=true;
                 ELSE IsValid:=false;
  END IF;
END CHECKATRRELATIONVALIDITYPERIOD;

/
--------------------------------------------------------
--  DDL for Procedure CHECKATTRVALIDITY
--------------------------------------------------------
set define off;

  CREATE OR REPLACE PROCEDURE "NA62_CONDDB_ADMIN"."CHECKATTRVALIDITY" 
(
  Detector_prefix IN VARCHAR2,
  SubSystemTypeName IN VARCHAR2,
  AttributeID IN integer,  
  IsValid OUT boolean
) IS
TableName VARCHAR2(255) := '';
SQLCommand VARCHAR2(1000) := '';
RowsCount integer;
BEGIN
  IsValid:=false;
  TableName:=Detector_prefix || '_RelationshipValidity';
  SQLCommand := 'SELECT count(*) FROM "'||TableName||'" WHERE (
             (to_date(RelationshipValidityStart, "YYYY-MM-DD HH24:MI:SS")  
              <= sysdate) AND 
              (to_date(RelationshipValidityFinish, "YYYY-MM-DD HH24:MI:SS")  
              >= sysdate) AND AttributeID=:a AND SubSystemTypeName=:b)';
              
  execute immediate SQLCommand into RowsCount using AttributeID, SubSystemTypeName;
  IF RowsCount>0 THEN IsValid:=true;
                 ELSE IsValid:=false;
  END IF;
END CHECKATTRVALIDITY;

/
--------------------------------------------------------
--  DDL for Procedure CREATETMPTDATAVAL
--------------------------------------------------------
set define off;

  CREATE OR REPLACE PROCEDURE "NA62_CONDDB_ADMIN"."CREATETMPTDATAVAL" 
(
  Detector_prefix IN VARCHAR2  
) IS
TableName VARCHAR2(255) := '';
SQLCommand VARCHAR2(5000) := '';
 
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

SQLCommand:='CREATE OR REPLACE FORCE VIEW "'||Detector_prefix||'_CONSOLIDATEVIEW" ("ID", "SUBSYSTEMID", "ATTRIBUTENAME", "ATTRIBUTEID", "STORAGEDATATYPE", "SUBSYSTEMTYPENAME", "TIMEOFVALADDING", "VALUE", "VALUEVALIDITYSTART", "VALUEVALIDITYFINISH") AS 
  SELECT 
          CAST(ROWNUM AS NUMBER(9)) AS "ID",
          CAST("'||Detector_prefix||'_Values"."SubSystemID" AS NUMBER(9)) AS "SUBSYSTEMID",
          "'||Detector_prefix||'_Attributies"."Name" AS "ATTRIBUTENAME",
          "'||Detector_prefix||'_Attributies"."AttributeID" AS "ATTRIBUTEID",
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
execute immediate SQLCommand;



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



END CREATETMPTDATAVAL;






/*
SQLCommand:='
CREATE GLOBAL TEMPORARY
TABLE '||TableName||'  
(    
ID NUMBER(32),       
SUBSYSTEMTYPENAME VARCHAR2(255),
SUBSYSTEMID NUMBER(20,0),
ATTRIBUTENAME VARCHAR2(255),
VALUE BLOB,
ValueValidityStart TIMESTAMP (0),
ValueValidityFinish TIMESTAMP (0),
STORAGEDATATYPE VARCHAR2(255)
) 
ON COMMIT PRESERVE ROWS';
*/

/
--------------------------------------------------------
--  DDL for Procedure FILL_TMPDATAVAL
--------------------------------------------------------
set define off;

  CREATE OR REPLACE PROCEDURE "NA62_CONDDB_ADMIN"."FILL_TMPDATAVAL" 
(
  Detector_prefix IN VARCHAR2,
  StartTime IN TimeStamp,
  FinishTime IN TimeStamp,
  FetchedCount OUT integer
) AS 

TYPE cur_typ IS REF CURSOR;
c1 cur_typ;
attr_id integer;
TmpTableName VARCHAR2(255) := '';
AttrRelTableName VARCHAR2(255) := '';
ValTableName VARCHAR2(255) := '';
AttrTableName VARCHAR2(255) := '';
AttrDataTypeName VARCHAR2(255) := '';
SubSystemTableName VARCHAR2(255) := '';
TableName VARCHAR2(255) := '';
SQLCommand VARCHAR2(10000) := '';
RelationValidityStart TimeStamp;
RelationValidityFinish TimeStamp;
SUBSYSTEMID integer;
ATTRIBUTENAME VARCHAR2(255) := '';
STORAGEDATATYPE VARCHAR2(255) := '';
SUBSYSTEMTYPENAME VARCHAR2(255) := '';
VAL BLOB;
ValueValidityStart TimeStamp;
ValueValidityFinish TimeStamp;
FillValueValidityStart TIMESTAMP; 
FillValueValidityFinish TIMESTAMP;
TimeOfValAdding TIMESTAMP;
counter integer := 0;
BEGIN

   TMpTableName:=Detector_prefix || '_TMPDATAVAL';

 BEGIN
  execute immediate 'DELETE FROM '||TMpTableName;
  EXCEPTION
  WHEN OTHERS THEN
  NULL;
 END;



   AttrRelTableName:=Detector_prefix || '_RelationshipValidity';
   ValTableName:=Detector_prefix || '_Values';
   AttrTableName:=Detector_prefix || '_Attributies';
   AttrDataTypeName:=Detector_prefix || '_DataType';
   SubSystemTableName:=Detector_prefix || '_DetectorSubSystem';

/*
   SQLCommand :='
   SELECT "'||AttrRelTableName||'"."RelationValidityStart" AS "RelationValidityStart",
          "'||AttrRelTableName||'"."RelationValidityFinish" AS "RelationValidityFinish",
          "'||ValTableName||'"."SubSystem_ID" AS "SUBSYSTEMID",
          "'||AttrTableName||'"."Name" AS "ATTRIBUTENAME",
          "'||AttrDataTypeName||'"."StorageDataType" AS "STORAGEDATATYPE",
          "'||AttrRelTableName||'"."SubSystemTypeName" AS "SUBSYSTEMTYPENAME",
          "'||ValTableName||'"."Value" AS "VAL",
          "'||ValTableName||'"."ValueValidityStart" AS "ValueValidityStart",
          "'||ValTableName||'"."ValueValidityFinish" AS "ValueValidityFinish"
   FROM "'||AttrRelTableName||'", "'||AttrTableName||'", "'||AttrDataTypeName||'", "'||ValTableName||'" 
   WHERE (
     ("'||AttrRelTableName||'"."RelationValidityFinish" > :1) AND ("'||AttrRelTableName||'"."RelationValidityStart" < :2) AND 
     ("'||AttrTableName||'"."ID" = "'||AttrRelTableName||'"."Attributy_ID") AND 
     ("'||AttrTableName||'"."DataTypeName" = "'||AttrRelTableName||'"."DataTypeName") AND 
     ("'||ValTableName||'"."Attributy_ID" = "'||AttrRelTableName||'"."Attributy_ID") AND 
     ("'||ValTableName||'"."ValueValidityFinish" > :1) AND ("'||ValTableName||'"."ValueValidityStart" < :2)  
     )';
*/



   SQLCommand :='
   SELECT "'||AttrRelTableName||'"."RelationshipValidityStart" AS "RelationshipValidityStart",
          "'||AttrRelTableName||'"."RelationshipValidityFinish" AS "RelationshipValidityFinish",
          "'||ValTableName||'"."SubSystemID" AS "SUBSYSTEMID",
          "'||AttrTableName||'"."Name" AS "ATTRIBUTENAME",
          "'||AttrDataTypeName||'"."StorageDataType" AS "STORAGEDATATYPE",
          "'||AttrRelTableName||'"."SubSystemTypeName" AS "SUBSYSTEMTYPENAME",
          "'||ValTableName||'"."TimeOfValAdding" AS "TimeOfValAdding",
          "'||ValTableName||'"."ValueContent" AS "VAL",
          "'||ValTableName||'"."ValueValidityStart" AS "ValueValidityStart",
          "'||ValTableName||'"."ValueValidityFinish" AS "ValueValidityFinish"
   FROM "'||AttrRelTableName||'", "'||AttrTableName||'", "'||AttrDataTypeName||'", "'||ValTableName||'", "'||SubSystemTableName||'"  
   WHERE (
     ("'||AttrRelTableName||'"."RelationshipValidityFinish" > :1) AND ("'||AttrRelTableName||'"."RelationshipValidityStart" < :2) AND 
     ("'||AttrTableName||'"."AttributeID" = "'||AttrRelTableName||'"."AttributeID") AND
     ("'||AttrTableName||'"."DataTypeName" = "'||AttrDataTypeName||'"."DataTypeName") AND 
     ("'||ValTableName||'"."AttributeID" = "'||AttrRelTableName||'"."AttributeID")  AND
     ("'||ValTableName||'"."SubSystemID" = "'||SubSystemTableName||'"."SubSystemID")  AND
     ("'||AttrRelTableName||'"."SubSystemTypeName" = "'||SubSystemTableName||'"."SubSystemTypeName")  AND
     ("'||ValTableName||'"."ValueValidityFinish" > :3) AND ("'||ValTableName||'"."ValueValidityStart" < :4)  
     )';

     
/*
   SQLCommand :='
   SELECT "'||AttrRelTableName||'"."RelationValidityStart" AS "RelationValidityStart"
   FROM "'||AttrRelTableName||'"';
*/   
   OPEN c1 FOR SQLCommand USING StartTime, FinishTime, StartTime, FinishTime;





   LOOP   
    FETCH c1 INTO RelationValidityStart, RelationValidityFinish, SUBSYSTEMID, 
      ATTRIBUTENAME, STORAGEDATATYPE, SUBSYSTEMTYPENAME, TimeOfValAdding, VAL, ValueValidityStart, ValueValidityFinish;
   
    EXIT WHEN c1%NOTFOUND;

    IF (RelationValidityStart>ValueValidityStart) THEN 
      FillValueValidityStart:=RelationValidityStart;
    ELSE
      FillValueValidityStart:=ValueValidityStart;
    END IF;
  
    IF (RelationValidityFinish>ValueValidityFinish) THEN 
      FillValueValidityFinish:=ValueValidityFinish;
    ELSE
      FillValueValidityFinish:=RelationValidityFinish;
    END IF;
  
 
   SQLCommand := 'INSERT INTO "'||TMpTableName||'" (
   "ID", "SUBSYSTEMTYPENAME", "SUBSYSTEMID", "ATTRIBUTENAME", "VALUE", "TIMEOFVALADDING",
   "VALUEVALIDITYSTART", "VALUEVALIDITYFINISH", "STORAGEDATATYPE") 
   values(:1, :2, :3, :4, :5, :6, :7, :8, :9)';
   
   execute immediate SQLCommand using counter, SUBSYSTEMTYPENAME, SUBSYSTEMID, 
      ATTRIBUTENAME, VAL, TimeOfValAdding, ValueValidityStart, ValueValidityFinish, STORAGEDATATYPE;
   counter:=counter+1;
   END LOOP;
   CLOSE c1;
   FetchedCount:=counter;
END FILL_TMPDATAVAL;

/
--------------------------------------------------------
--  DDL for Procedure FINISHVALUEVALIDITY
--------------------------------------------------------
set define off;

  CREATE OR REPLACE PROCEDURE "NA62_CONDDB_ADMIN"."FINISHVALUEVALIDITY" 
(
  Detector_prefix IN VARCHAR2,
  ValueID IN integer,
  TimeAtFinish IN timestamp
) IS
TableName VARCHAR2(255) := '';
SQLCommand VARCHAR2(1000) := '';

BEGIN
  TableName:=Detector_prefix || '_Values';
  SQLCommand:='UPDATE "'||TableName||'" SET ValueValidityFinish=:1 WHERE ValueID=:2';
  execute immediate SQLCommand using TimeAtFinish, ValueID;
END FINISHVALUEVALIDITY;

/
--------------------------------------------------------
--  DDL for Procedure GET_ATTR_ID_BY_NAME
--------------------------------------------------------
set define off;

  CREATE OR REPLACE PROCEDURE "NA62_CONDDB_ADMIN"."GET_ATTR_ID_BY_NAME" 
(
  Detector_prefix IN VARCHAR2,
  Attribute_Name IN VARCHAR2,  
  AttributeID OUT integer,
  SubSystemID IN integer
) IS
TYPE cur_typ IS REF CURSOR;

TYPE attribsarraytype IS VARRAY (200) of integer;
AttribsArray attribsarraytype:= attribsarraytype();
AttribsArrayVal attribsarraytype:= attribsarraytype();
SubSystemTypeName VARCHAR2(255):= '';
TableName VARCHAR2(255) := '';
SQLCommand VARCHAR2(1000) := '';
v_cursor VARCHAR2(255);
attr_name VARCHAR2(255);
attr_id integer;
cu cur_typ;
c1 cur_typ;

counter integer :=1;
counter1 integer :=1;

BEGIN


   AttributeID:=-1;
   attr_id:=-1;
   TableName:=Detector_prefix || '_Attributies';
   SQLCommand := 'SELECT "'||TableName||'"."AttributeID" AS "AttributeID", "'||TableName||'"."Name" AS "Name"  FROM "'||TableName||'"';
   OPEN cu FOR SQLCommand;
   LOOP
    FETCH cu INTO attr_id,attr_name;
    IF trim(attr_name)=Attribute_Name THEN 
      AttribsArray.extend;
      AttribsArray(counter):=attr_id;
      counter:=counter+1;
    END IF;
   EXIT WHEN cu%NOTFOUND;
   END LOOP;
   CLOSE cu;

   TableName:=Detector_prefix || '_RelationshipValidity';
   SQLCommand := 'SELECT "AttributeID" FROM "'||TableName||'" WHERE ("SubSystemTypeName"=:1)';
   GETSUBSYSTEMTYPENAMEBYID(Detector_prefix,SubSystemID, SubSystemTypeName); 
   OPEN c1 FOR SQLCommand USING SubSystemTypeName;
   LOOP
    FETCH c1 INTO attr_id;
      AttribsArrayVal.extend;
      AttribsArrayVal(counter1):=attr_id;
      counter1:=counter1+1;
   EXIT WHEN c1%NOTFOUND;
   END LOOP;
   CLOSE c1;

   for i in 1..(counter-1) loop
     for j in 1..(counter1-1) loop
      if AttribsArray(i) = AttribsArrayVal(j) then
        AttributeID :=AttribsArray(i);
      END IF;
     end loop;
   end loop;
   
 END GET_ATTR_ID_BY_NAME;  
  
  
  


/*
 TableName:=Detector_prefix || '_RelationshipValidity';
  SQLCommand := 'SELECT count(*) FROM "'||TableName||'" WHERE (
             ("RelationshipValidityStart" <= :1) AND 
              ("RelationshipValidityFinish" >= :2) AND ("AttributeID"=:3) AND 
              ("SubSystemTypeName"=:4))';
              
  execute immediate SQLCommand into RowsCount using StartPeriod, FinishPeriod, 
              AttributeID, SubSystemTypeName;
  
  IF RowsCount>0 THEN IsValid:=true;
                 ELSE IsValid:=false;
  END IF;
*/


/*
create or replace
PROCEDURE          "GET_ATTR_ID_BY_NAME" 
(
  Detector_prefix IN VARCHAR2,
  Attribute_Name IN VARCHAR2,  
  SubSystemCount OUT integer
) IS
TYPE cur_typ IS REF CURSOR;
TableName VARCHAR2(255) := '';
SQLCommand VARCHAR2(1000) := '';
v_cursor VARCHAR2(255);
attr_name VARCHAR2(255);
c cur_typ;
counter integer;
BEGIN
   counter:=0;
   TableName:=Detector_prefix || '_Attributies';
   SQLCommand :='SELECT "Name" FROM "'||TableName||'"';
   OPEN c FOR SQLCommand;
   LOOP
    FETCH c INTO attr_name;
    IF trim(attr_name)=Attribute_Name THEN EXIT;
    END IF;
    counter:=counter+1;
   EXIT WHEN c%NOTFOUND;
   END LOOP;
   CLOSE c;
   SubSystemCount:=counter;
END GET_ATTR_ID_BY_NAME;
*/

/
--------------------------------------------------------
--  DDL for Procedure GET_ATTR_POS_COUNT_BY_NAME
--------------------------------------------------------
set define off;

  CREATE OR REPLACE PROCEDURE "NA62_CONDDB_ADMIN"."GET_ATTR_POS_COUNT_BY_NAME" 
(
  Detector_prefix IN VARCHAR2,
  Attribute_Name IN VARCHAR2,  
  SubSystemCount OUT integer
) IS
TYPE cur_typ IS REF CURSOR;
TableName VARCHAR2(255) := '';
SQLCommand VARCHAR2(1000) := '';
attr_name VARCHAR2(255);
c cur_typ;
counter integer;
BEGIN
   counter:=0;
   TableName:=Detector_prefix || '_Attributies';
   SQLCommand :='SELECT "Name" FROM "'||TableName||'"';
   OPEN c FOR SQLCommand;
   LOOP
    FETCH c INTO attr_name;
    IF trim(attr_name)=Attribute_Name THEN EXIT;
    END IF;
    counter:=counter+1;
   EXIT WHEN c%NOTFOUND;
   END LOOP;
   CLOSE c;
   SubSystemCount:=counter;
END GET_ATTR_POS_COUNT_BY_NAME;

/
--------------------------------------------------------
--  DDL for Procedure GETSUBSYSTEMSCOUNT_STORED
--------------------------------------------------------
set define off;

  CREATE OR REPLACE PROCEDURE "NA62_CONDDB_ADMIN"."GETSUBSYSTEMSCOUNT_STORED" 
(
  Detector_prefix IN VARCHAR2,
  SubSystemTypeName1 IN VARCHAR2,  
  SubSystemCount OUT integer
) IS
TableName VARCHAR2(255) := '';
SQLCommand VARCHAR2(1000) := '';
BEGIN
    SubSystemCount:=0;
    TableName:=Detector_prefix || '_DetectorSubSystem';
    SQLCommand := 'SELECT count(*) FROM "'||TableName||'" where trim("SubSystemTypeName") = :a';
    execute immediate SQLCommand into SubSystemCount using SubSystemTypeName1;
    
END GETSUBSYSTEMSCOUNT_STORED;

/
--------------------------------------------------------
--  DDL for Procedure GETSUBSYSTEMTYPENAMEBYID
--------------------------------------------------------
set define off;

  CREATE OR REPLACE PROCEDURE "NA62_CONDDB_ADMIN"."GETSUBSYSTEMTYPENAMEBYID" 
(
  Detector_prefix IN VARCHAR2,
  SubSystemID IN integer,
  SubSystemTypeName OUT VARCHAR2
) IS
TYPE cur_typ IS REF CURSOR;
TableName VARCHAR2(255) := '';
SQLCommand VARCHAR2(1000) := '';
c cur_typ;
BEGIN
   SubSystemTypeName:='';
   TableName:=Detector_prefix || '_DetectorSubSystem';
   SQLCommand :='SELECT "SubSystemTypeName" FROM "'||TableName||'" WHERE "SubSystemID"=:1';
   OPEN c FOR SQLCommand USING SubSystemID;
   LOOP
    FETCH c INTO SubSystemTypeName;
   EXIT WHEN c%NOTFOUND;
   END LOOP;
   CLOSE c;
END GETSUBSYSTEMTYPENAMEBYID;

/
--------------------------------------------------------
--  DDL for Procedure GETSUBSYSTEMTYPESCOUNT_STORED
--------------------------------------------------------
set define off;

  CREATE OR REPLACE PROCEDURE "NA62_CONDDB_ADMIN"."GETSUBSYSTEMTYPESCOUNT_STORED" 
(
  Detector_prefix IN VARCHAR2,
  SubSystemTypesCount OUT integer
) IS
TableName VARCHAR2(255) := '';
SQLCommand VARCHAR2(1000) := '';
BEGIN 
    SubSystemTypesCount:=0;
    TableName:=Detector_prefix || '_SubSystemType';
    SQLCommand := 'SELECT count(*) FROM "'||TableName||'"';
    execute immediate SQLCommand into SubSystemTypesCount;
END GETSUBSYSTEMTYPESCOUNT_STORED;

/
--------------------------------------------------------
--  DDL for Procedure SETATTRIBUTEVALUE
--------------------------------------------------------
set define off;

  CREATE OR REPLACE PROCEDURE "NA62_CONDDB_ADMIN"."SETATTRIBUTEVALUE" 
(
  Detector_prefix IN VARCHAR2,
  StoredValue IN BLOB,
  AttributeName IN VARCHAR2,
  StartTime IN TimeStamp,
  FinishTime IN TimeStamp,
  OutResult OUT integer,
  SubSystem_ID IN integer
) IS
TYPE cur_typ IS REF CURSOR;
attr_id integer;
invalid_attribute exception;
IsValid boolean;
c cur_typ;
TableName VARCHAR2(255) := '';
SQLCommand VARCHAR2(20000) := '';
SubSystemTypeName VARCHAR2(255) := '';
ValID integer;
CountVal integer;
BEGIN

   attr_id:=-1;
   GET_ATTR_ID_BY_NAME(Detector_prefix,AttributeName,attr_id, SubSystem_ID);
   IF attr_id=-1 THEN 
     raise invalid_attribute;
   END IF;
   
   IsValid:=false;
   
   GETSUBSYSTEMTYPENAMEBYID(Detector_prefix, SubSystem_ID, SubSystemTypeName);
   CHECKATRRELAIONVALODITYPERIOD(Detector_prefix, SubSystemTypeName, 
                                 attr_id, StartTime,FinishTime, IsValid);
   IF IsValid=false THEN 
     raise invalid_attribute;
   END IF;


   TableName:=Detector_prefix || '_Values';
   SQLCommand := 'SELECT count(*) FROM "'||TableName||'"';
   execute immediate SQLCommand into CountVal;

   execute immediate 'INSERT INTO "'||TableName||'" ("ValueID", "SubSystemID", 
   "TimeOfValAdding",
   "AttributeID", "ValueContent", "ValueValidityStart", "ValueValidityFinish") 
   values(:1, :2, :3, :4, :5, :6, :7)' using CountVal, SubSystem_ID, sysdate,
                  attr_id, StoredValue, StartTime, FinishTime;




exception
   when invalid_attribute then
        OutResult:=-1;
        
END SETATTRIBUTEVALUE;

/
