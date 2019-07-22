--------------------------------------------------------
--  File created - Monday-November-25-2013   
--------------------------------------------------------
--------------------------------------------------------
--  DDL for View GTK_AttrDataTypeView
--------------------------------------------------------

  CREATE OR REPLACE FORCE VIEW "NA62_CONDDB_ADMIN"."GTK_AttrDataTypeView" ("StorageDataType", "AttrName", "SubSystemTypeName", "RelationshipValidityStart", "RelationshipValidityFinish") AS 
  ( SELECT "GTK_DataType"."StorageDataType" AS "StorageDataType", "GTK_Attributies"."Name" AS "AttrName", "GTK_RelationshipValidity"."SubSystemTypeName" AS "SubSystemTypeName", "GTK_RelationshipValidity"."RelationshipValidityStart" AS "RelationshipValidityStart", "GTK_RelationshipValidity"."RelationshipValidityFinish" AS "RelationshipValidityFinish" FROM NA62_CONDDB_ADMIN."GTK_DataType" "GTK_DataType", NA62_CONDDB_ADMIN."GTK_Attributies" "GTK_Attributies", NA62_CONDDB_ADMIN."GTK_RelationshipValidity" "GTK_RelationshipValidity" WHERE "GTK_DataType"."DataTypeName" = "GTK_Attributies"."DataTypeName" AND "GTK_RelationshipValidity"."AttributeID" = "GTK_Attributies"."AttributeID" );
--------------------------------------------------------
--  DDL for View GTK_CONSOLIDATEVIEW
--------------------------------------------------------

  CREATE OR REPLACE FORCE VIEW "NA62_CONDDB_ADMIN"."GTK_CONSOLIDATEVIEW" ("ID", "RelationshipValidityStart", "RelationshipValidityFinish", "SUBSYSTEMID", "ATTRIBUTENAME", "STORAGEDATATYPE", "SUBSYSTEMTYPENAME", "TIMEOFVALADDING", "VALUE", "VALUEVALIDITYSTART", "VALUEVALIDITYFINISH") AS 
  SELECT 
          ROWNUM AS "ID",
          "GTK_RelationshipValidity"."RelationshipValidityStart" AS "RelationshipValidityStart",
          "GTK_RelationshipValidity"."RelationshipValidityFinish" AS "RelationshipValidityFinish",
          "GTK_Values"."SubSystemID" AS "SUBSYSTEMID",
          "GTK_Attributies"."Name" AS "ATTRIBUTENAME",
          "GTK_DataType"."StorageDataType" AS "STORAGEDATATYPE",
          "GTK_RelationshipValidity"."SubSystemTypeName" AS "SUBSYSTEMTYPENAME",
          "GTK_Values"."TimeOfValAdding" AS "TIMEOFVALADDING",
          "GTK_Values"."ValueContent" AS "VALUE",

           CASE WHEN  "GTK_Values"."ValueValidityStart" >= "GTK_RelationshipValidity"."RelationshipValidityStart" THEN "GTK_Values"."ValueValidityStart"
                   WHEN  "GTK_Values"."ValueValidityStart" < "GTK_RelationshipValidity"."RelationshipValidityStart" THEN "GTK_RelationshipValidity"."RelationshipValidityStart"
           END AS "VALUEVALIDITYSTART",
          
           CASE WHEN  "GTK_Values"."ValueValidityFinish" <= "GTK_RelationshipValidity"."RelationshipValidityFinish" THEN "GTK_Values"."ValueValidityFinish"
                     WHEN  "GTK_Values"."ValueValidityFinish" > "GTK_RelationshipValidity"."RelationshipValidityFinish" THEN "GTK_RelationshipValidity"."RelationshipValidityFinish"
           END AS "VALUEVALIDITYFINISH"

   FROM "GTK_RelationshipValidity", "GTK_Attributies", "GTK_DataType", "GTK_Values", "GTK_DetectorSubSystem"  
   WHERE 
      ("GTK_Attributies"."AttributeID" = "GTK_RelationshipValidity"."AttributeID") AND
     ("GTK_Attributies"."DataTypeName" = "GTK_DataType"."DataTypeName") AND 
     ("GTK_Values"."AttributeID" = "GTK_RelationshipValidity"."AttributeID")  AND
     ("GTK_Values"."SubSystemID" = "GTK_DetectorSubSystem"."SubSystemID")  AND
     ("GTK_RelationshipValidity"."SubSystemTypeName" = "GTK_DetectorSubSystem"."SubSystemTypeName");
--------------------------------------------------------
--  DDL for View GTK_CORALAVLOIDERSST0
--------------------------------------------------------

  CREATE OR REPLACE FORCE VIEW "NA62_CONDDB_ADMIN"."GTK_CORALAVLOIDERSST0" ("SSTCOUNT") AS 
  SELECT 
   CAST(COUNT(*) AS NUMBER(9)) AS "SSTCOUNT"
FROM
   "GTK_SubSystemType";
--------------------------------------------------------
--  DDL for View GTK_CORALAVLOIDERSSTC1
--------------------------------------------------------

  CREATE OR REPLACE FORCE VIEW "NA62_CONDDB_ADMIN"."GTK_CORALAVLOIDERSSTC1" ("SUBSYSTEMTYPENAME", "SUBSYSTEMCOUNT") AS 
  SELECT 
  "GTK_DetectorSubSystem"."SubSystemTypeName"  AS "SUBSYSTEMTYPENAME",
  CAST (COUNT("GTK_DetectorSubSystem"."SubSystemTypeName") AS NUMBER(9)) AS "SUBSYSTEMCOUNT"
FROM
   "GTK_DetectorSubSystem"
GROUP BY  "GTK_DetectorSubSystem"."SubSystemTypeName";
REM INSERTING into NA62_CONDDB_ADMIN."GTK_AttrDataTypeView"
SET DEFINE OFF;
Insert into NA62_CONDDB_ADMIN."GTK_AttrDataTypeView" ("StorageDataType","AttrName","SubSystemTypeName","RelationshipValidityStart","RelationshipValidityFinish") values ('long long                                         ','DBID                                              ','Test                                              ',to_timestamp('01-JAN-00 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('01-JAN-99 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'));
Insert into NA62_CONDDB_ADMIN."GTK_AttrDataTypeView" ("StorageDataType","AttrName","SubSystemTypeName","RelationshipValidityStart","RelationshipValidityFinish") values ('multitype                                         ','fSerialization1                                   ','Test                                              ',to_timestamp('01-JAN-00 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('01-JAN-99 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'));
Insert into NA62_CONDDB_ADMIN."GTK_AttrDataTypeView" ("StorageDataType","AttrName","SubSystemTypeName","RelationshipValidityStart","RelationshipValidityFinish") values ('long long                                         ','fNative1                                          ','Test                                              ',to_timestamp('01-JAN-00 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('01-JAN-99 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'));
Insert into NA62_CONDDB_ADMIN."GTK_AttrDataTypeView" ("StorageDataType","AttrName","SubSystemTypeName","RelationshipValidityStart","RelationshipValidityFinish") values ('multitype                                         ','fSerialization2                                   ','Test                                              ',to_timestamp('01-JAN-00 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('01-JAN-99 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'));
Insert into NA62_CONDDB_ADMIN."GTK_AttrDataTypeView" ("StorageDataType","AttrName","SubSystemTypeName","RelationshipValidityStart","RelationshipValidityFinish") values ('long long                                         ','fNative2                                          ','Test                                              ',to_timestamp('01-JAN-00 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('01-JAN-99 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'));
Insert into NA62_CONDDB_ADMIN."GTK_AttrDataTypeView" ("StorageDataType","AttrName","SubSystemTypeName","RelationshipValidityStart","RelationshipValidityFinish") values ('multitype                                         ','fSerialization3                                   ','Test                                              ',to_timestamp('01-JAN-00 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('01-JAN-99 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'));
Insert into NA62_CONDDB_ADMIN."GTK_AttrDataTypeView" ("StorageDataType","AttrName","SubSystemTypeName","RelationshipValidityStart","RelationshipValidityFinish") values ('long long                                         ','fNative3                                          ','Test                                              ',to_timestamp('01-JAN-00 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('01-JAN-99 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'));
Insert into NA62_CONDDB_ADMIN."GTK_AttrDataTypeView" ("StorageDataType","AttrName","SubSystemTypeName","RelationshipValidityStart","RelationshipValidityFinish") values ('multitype                                         ','fSerialization4                                   ','Test                                              ',to_timestamp('01-JAN-00 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('01-JAN-99 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'));
Insert into NA62_CONDDB_ADMIN."GTK_AttrDataTypeView" ("StorageDataType","AttrName","SubSystemTypeName","RelationshipValidityStart","RelationshipValidityFinish") values ('long long                                         ','fNative4                                          ','Test                                              ',to_timestamp('01-JAN-00 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('01-JAN-99 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'));
REM INSERTING into NA62_CONDDB_ADMIN.GTK_CONSOLIDATEVIEW
SET DEFINE OFF;
Insert into NA62_CONDDB_ADMIN.GTK_CONSOLIDATEVIEW (ID,"RelationshipValidityStart","RelationshipValidityFinish",SUBSYSTEMID,ATTRIBUTENAME,STORAGEDATATYPE,SUBSYSTEMTYPENAME,TIMEOFVALADDING,VALUEVALIDITYSTART,VALUEVALIDITYFINISH) values (1,to_timestamp('01-JAN-00 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('01-JAN-99 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),0,'DBID                                              ','long long                                         ','Test                                              ',to_timestamp('23-NOV-13 02.48.17.000000000 PM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('23-NOV-11 01.48.16.000000000 PM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('01-JAN-38 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'));
Insert into NA62_CONDDB_ADMIN.GTK_CONSOLIDATEVIEW (ID,"RelationshipValidityStart","RelationshipValidityFinish",SUBSYSTEMID,ATTRIBUTENAME,STORAGEDATATYPE,SUBSYSTEMTYPENAME,TIMEOFVALADDING,VALUEVALIDITYSTART,VALUEVALIDITYFINISH) values (2,to_timestamp('01-JAN-00 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('01-JAN-99 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),0,'fSerialization1                                   ','multitype                                         ','Test                                              ',to_timestamp('23-NOV-13 02.48.18.000000000 PM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('10-AUG-12 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('10-SEP-12 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'));
Insert into NA62_CONDDB_ADMIN.GTK_CONSOLIDATEVIEW (ID,"RelationshipValidityStart","RelationshipValidityFinish",SUBSYSTEMID,ATTRIBUTENAME,STORAGEDATATYPE,SUBSYSTEMTYPENAME,TIMEOFVALADDING,VALUEVALIDITYSTART,VALUEVALIDITYFINISH) values (3,to_timestamp('01-JAN-00 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('01-JAN-99 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),0,'fSerialization1                                   ','multitype                                         ','Test                                              ',to_timestamp('23-NOV-13 02.48.19.000000000 PM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('20-SEP-12 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('20-OCT-12 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'));
Insert into NA62_CONDDB_ADMIN.GTK_CONSOLIDATEVIEW (ID,"RelationshipValidityStart","RelationshipValidityFinish",SUBSYSTEMID,ATTRIBUTENAME,STORAGEDATATYPE,SUBSYSTEMTYPENAME,TIMEOFVALADDING,VALUEVALIDITYSTART,VALUEVALIDITYFINISH) values (4,to_timestamp('01-JAN-00 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('01-JAN-99 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),0,'fSerialization2                                   ','multitype                                         ','Test                                              ',to_timestamp('23-NOV-13 02.48.20.000000000 PM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('20-JUL-12 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('10-SEP-12 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'));
Insert into NA62_CONDDB_ADMIN.GTK_CONSOLIDATEVIEW (ID,"RelationshipValidityStart","RelationshipValidityFinish",SUBSYSTEMID,ATTRIBUTENAME,STORAGEDATATYPE,SUBSYSTEMTYPENAME,TIMEOFVALADDING,VALUEVALIDITYSTART,VALUEVALIDITYFINISH) values (5,to_timestamp('01-JAN-00 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('01-JAN-99 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),0,'fSerialization2                                   ','multitype                                         ','Test                                              ',to_timestamp('23-NOV-13 02.48.21.000000000 PM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('20-AUG-12 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('10-OCT-12 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'));
Insert into NA62_CONDDB_ADMIN.GTK_CONSOLIDATEVIEW (ID,"RelationshipValidityStart","RelationshipValidityFinish",SUBSYSTEMID,ATTRIBUTENAME,STORAGEDATATYPE,SUBSYSTEMTYPENAME,TIMEOFVALADDING,VALUEVALIDITYSTART,VALUEVALIDITYFINISH) values (6,to_timestamp('01-JAN-00 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('01-JAN-99 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),0,'fSerialization3                                   ','multitype                                         ','Test                                              ',to_timestamp('23-NOV-13 02.48.22.000000000 PM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('20-AUG-12 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('10-OCT-12 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'));
Insert into NA62_CONDDB_ADMIN.GTK_CONSOLIDATEVIEW (ID,"RelationshipValidityStart","RelationshipValidityFinish",SUBSYSTEMID,ATTRIBUTENAME,STORAGEDATATYPE,SUBSYSTEMTYPENAME,TIMEOFVALADDING,VALUEVALIDITYSTART,VALUEVALIDITYFINISH) values (7,to_timestamp('01-JAN-00 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('01-JAN-99 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),0,'fSerialization3                                   ','multitype                                         ','Test                                              ',to_timestamp('23-NOV-13 02.48.23.000000000 PM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('20-SEP-12 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('20-OCT-12 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'));
Insert into NA62_CONDDB_ADMIN.GTK_CONSOLIDATEVIEW (ID,"RelationshipValidityStart","RelationshipValidityFinish",SUBSYSTEMID,ATTRIBUTENAME,STORAGEDATATYPE,SUBSYSTEMTYPENAME,TIMEOFVALADDING,VALUEVALIDITYSTART,VALUEVALIDITYFINISH) values (8,to_timestamp('01-JAN-00 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('01-JAN-99 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),0,'fSerialization4                                   ','multitype                                         ','Test                                              ',to_timestamp('23-NOV-13 02.48.24.000000000 PM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('20-JUL-12 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('20-OCT-12 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'));
Insert into NA62_CONDDB_ADMIN.GTK_CONSOLIDATEVIEW (ID,"RelationshipValidityStart","RelationshipValidityFinish",SUBSYSTEMID,ATTRIBUTENAME,STORAGEDATATYPE,SUBSYSTEMTYPENAME,TIMEOFVALADDING,VALUEVALIDITYSTART,VALUEVALIDITYFINISH) values (9,to_timestamp('01-JAN-00 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('01-JAN-99 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),0,'fSerialization4                                   ','multitype                                         ','Test                                              ',to_timestamp('23-NOV-13 02.48.25.000000000 PM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('20-AUG-12 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'),to_timestamp('10-OCT-12 12.00.00.000000000 AM','DD-MON-RR HH.MI.SSXFF AM'));
REM INSERTING into NA62_CONDDB_ADMIN.GTK_CORALAVLOIDERSST0
SET DEFINE OFF;
Insert into NA62_CONDDB_ADMIN.GTK_CORALAVLOIDERSST0 (SSTCOUNT) values (13);
REM INSERTING into NA62_CONDDB_ADMIN.GTK_CORALAVLOIDERSSTC1
SET DEFINE OFF;
Insert into NA62_CONDDB_ADMIN.GTK_CORALAVLOIDERSSTC1 (SUBSYSTEMTYPENAME,SUBSYSTEMCOUNT) values ('Test                                              ',1);
