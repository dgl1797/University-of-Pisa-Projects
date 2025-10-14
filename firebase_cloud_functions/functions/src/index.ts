import { HttpStatusCodes } from "./http_codes";
import * as serviceAccount from "./adminsdk_permission.json";
import * as NodeGeocoder from "node-geocoder";
import * as functions from "firebase-functions";
import * as admin from "firebase-admin";
import * as express from "express";
import * as dotenv from "dotenv";
import * as cors from "cors";
import { GeoCoordinates, GeoPointWithDCI } from "./models/GeoTypes";
import { CellDoc, CityDoc, ZoneDoc } from "./models/CityDocs";
import { cid2str, emptyCellDoc, emptyCityDoc, emptyZoneDoc, relativeCentroid } from "./utils/Functions";
import { throwError } from "./utils/Errors";
import { cellLength } from "./utils/Constants";

dotenv.config();

admin.initializeApp({
  credential: admin.credential.cert(serviceAccount as admin.ServiceAccount),
});

const db = admin.firestore();
const geomaps = db.collection("geomaps");

const geocoder = NodeGeocoder({
  provider: "openstreetmap",
});
const app = express();

// MIDDLEWARES
app.use(cors({ origin: true }));
app.use(express.json());

// GET ROUTES
app.get("/geomap/:city", async (req, res) => {
  try {
    const { city } = req.params;
    if (!city) throwError(HttpStatusCodes.ClientError.BAD_REQUEST, "no city in the request");

    const cityDoc = await geomaps.doc(city.toLowerCase()).get();
    if (!cityDoc.exists) throwError(404, `You are the first rider in ${city} using Bike Assistant!`);

    return res.status(200).json({ data: cityDoc.data() ?? throwError(HttpStatusCodes.ServerError.INTERNAL_ERROR, "Got Empty Document") });
  } catch (e: any) {
    if (!!e.code && !!e.message) {
      const error = e as SimpleError;
      return res.status(error.code).json({
        message: error.message,
      });
    } else {
      console.error(e);
      return res.status(HttpStatusCodes.ServerError.INTERNAL_ERROR).json({
        message: e,
      });
    }
  }
});

// POST ROUTES
app.post("/geomap/:city/:zone", async (req, res) => {
  try {
    // PARSE REQUEST
    const { city, zone }: { city: string; zone: string } = req.params;
    if (!city || !zone) throwError(HttpStatusCodes.ClientError.BAD_REQUEST, "city or zone not specified");
    const route: GeoPointWithDCI[] = req.body.data;
    if (route.length === 0) throwError(HttpStatusCodes.ClientError.BAD_REQUEST, "expected at least 1 geopoint in route but got empty");

    // GET THE DOCUMENT BODY
    const cityDocRef = geomaps.doc(city.toLowerCase());
    const cityDoc: CityDoc =
      ((await cityDocRef.get())?.data() as CityDoc) ??
      emptyCityDoc(
        zone.toLowerCase(),
        (await geocoder.geocode(city)).find((el) => el.city?.toLowerCase() === city.toLowerCase()),
        (await geocoder.geocode(`${city}, ${zone}`)).find((el) => el.city?.toLowerCase() === city.toLowerCase())
      );

    // UPDATE CITY DOCUMENT
    let oldTot = cityDoc.avgDCI * cityDoc.nSamples;
    let routeTotalDCI = route.map((el) => el.dci).reduce((prev, curr) => prev + curr);
    cityDoc.nSamples += route.length;
    cityDoc.avgDCI = (oldTot + routeTotalDCI) / cityDoc.nSamples;

    // GET ZONE DOCUMENT
    const zoneDoc: ZoneDoc =
      cityDoc.zones.find((el) => el.name.toLowerCase() === zone.toLowerCase()) ??
      cityDoc.zones[
        cityDoc.zones.push(
          emptyZoneDoc(
            zone.toLowerCase(),
            (await geocoder.geocode(`${city}${zone.toLowerCase() === city.toLowerCase() ? "" : ", " + zone}`)).find(
              (el) => el.city?.toLowerCase() === city.toLowerCase()
            )
          )
        ) - 1
      ];

    // UPDATE ZONE DOCUMENT
    oldTot = zoneDoc.avgDCI * zoneDoc.nSamples;
    zoneDoc.nSamples += route.length;
    zoneDoc.avgDCI = (oldTot + routeTotalDCI) / zoneDoc.nSamples;

    // PARSE RECEIVED GEOPOINTS
    route
      .filter((r) => r.dci >= 0 && r.dci <= 1)
      .forEach((pt) => {
        // CALCULATE CENTROID OF POINT
        const point: GeoCoordinates = { latitude: pt.point.mLatitude, longitude: pt.point.mLongitude };
        const dci = pt.dci;
        const referenceCenter: GeoCoordinates = { latitude: zoneDoc.center.latitude, longitude: zoneDoc.center.longitude };
        const [centroid, cellID] = relativeCentroid(point, referenceCenter, cellLength);

        // GET CELL DOCUMENT
        const cellDoc: CellDoc =
          zoneDoc.cells.find((el) => el.cellID === cid2str(cellID)) ?? zoneDoc.cells[zoneDoc.cells.push(emptyCellDoc(cellID, centroid)) - 1];

        // UPDATE CELL DOCUMENT
        oldTot = cellDoc.avgDCI * cellDoc.nSamples++;
        cellDoc.avgDCI = (oldTot + dci) / cellDoc.nSamples;
      });

    await cityDocRef.set(cityDoc, { merge: true });
    return res.status(200).json({
      writtenData: cityDoc,
    });
  } catch (e: any) {
    if (!!e.code && !!e.message) {
      const error = e as SimpleError;
      return res.status(error.code).json({
        message: error.message,
      });
    } else {
      console.error(e);
      return res.status(HttpStatusCodes.ServerError.INTERNAL_ERROR).json({
        message: e,
      });
    }
  }
});

exports.apis = functions.region(process?.env?.REGION ?? "").https.onRequest(app);
