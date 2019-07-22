--------------------------------------------------------
--  File created - Tuesday-July-10-2012   
--------------------------------------------------------






-- Unable to render PROCEDURE DDL for object NA62DB.FINISHVALUEVALIDITY with DBMS_METADATA attempting internal generator.
CREATE PROCEDURE "FINISHVALUEVALIDITY" 
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
--------------------------------------------------------
--  DDL for Procedure GET_ATTR_ID_BY_NAME
--------------------------------------------------------
set define off;

  CREATE OR REPLACE PROCEDURE "NA62DB"."GET_ATTR_ID_BY_NAME" 
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

  CREATE OR REPLACE PROCEDURE "NA62DB"."GET_ATTR_POS_COUNT_BY_NAME" 
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

  CREATE OR REPLACE PROCEDURE "NA62DB"."GETSUBSYSTEMSCOUNT_STORED" 
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

  CREATE OR REPLACE PROCEDURE "NA62DB"."GETSUBSYSTEMTYPENAMEBYID" 
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

  CREATE OR REPLACE PROCEDURE "NA62DB"."GETSUBSYSTEMTYPESCOUNT_STORED" 
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

  CREATE OR REPLACE PROCEDURE "NA62DB"."SETATTRIBUTEVALUE" 
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



/*instead of calling finishvaluevalidity*/

   TableName:=Detector_prefix || '_Values';
   SQLCommand:='UPDATE "'||TableName||'" SET "ValueValidityFinish"=:1 WHERE (
   ("ValueValidityFinish" > :2) AND ("ValueValidityStart" < :3)
   AND "AttributeID"=:4 AND "SubSystemID"=:5)';
   
   
   execute immediate SQLCommand using StartTime, StartTime, FinishTime, attr_id, SubSystem_ID;



   SQLCommand := 'SELECT count(*) FROM "'||TableName||'"';
   execute immediate SQLCommand into CountVal;

   execute immediate 'INSERT INTO "'||TableName||'" ("ValueID", "SubSystemID", 
   "AttributeID", "ValueContent", "ValueValidityStart", "ValueValidityFinish") 
   values(:1, :2, :3, :4, :5, :6)' using CountVal, SubSystem_ID, 
                  attr_id, StoredValue, StartTime, FinishTime;




exception
   when invalid_attribute then
        OutResult:=-1;
        
END SETATTRIBUTEVALUE;

/
