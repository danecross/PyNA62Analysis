// A script to visualize the output of scripts/G4GeometryToRoot.sh
// Further documentation available inside scripts/G4GeometryToRoot.sh
// Check that the path to NA62.root created by G4GeometryToRoot.sh is correct.
// Then run this script with "root -l -x scripts/CheckGeom.C"

{
    TEveManager::Create();
    gEve->GetMainWindow()->SetWindowName("NA62 Event Display");

    gGeoManager = gEve->GetGeometry("NA62.root");

    TGeoNode* NA62World = gGeoManager->GetTopNode();
    TEveGeoTopNode* NA62EveWorld = new TEveGeoTopNode(gGeoManager, NA62World);
    gEve->AddGlobalElement(NA62EveWorld);
}
