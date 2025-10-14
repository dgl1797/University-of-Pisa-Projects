import { CellDoc, CityDoc, ZoneDoc } from "../models/CityDocs";
import { CellID, GeoCoordinates } from "../models/GeoTypes";
import { cellLength } from "./Constants";
import { throwError } from "./Errors";
import * as NodeGeocoder from "node-geocoder";

function emptyCityDoc(zoneName: string, cityReference?: NodeGeocoder.Entry, zoneReference?: NodeGeocoder.Entry): CityDoc {
  if (!cityReference) throwError(404, "city or zone not found or not compatible");
  if (!zoneReference) zoneReference = cityReference;
  return {
    center: {
      latitude: cityReference.latitude ?? throwError(404, "parameter latitude is missing"),
      longitude: cityReference.longitude ?? throwError(404, "parameter longitude is missing"),
    },
    avgDCI: 0,
    nSamples: 0,
    zones: [
      {
        center: {
          latitude: zoneReference.latitude ?? throwError(404, "parameter latitude is missing"),
          longitude: zoneReference.longitude ?? throwError(404, "parameter longitude is missing"),
        },
        avgDCI: 0,
        nSamples: 0,
        cells: [],
        name: zoneName,
      },
    ],
  };
}

function emptyZoneDoc(zoneName: string, zoneReference?: NodeGeocoder.Entry): ZoneDoc {
  if (!zoneReference) throwError(404, "zone not present for the received city");
  return {
    center: {
      latitude: zoneReference.latitude ?? throwError(404, "parameter latitude is missing"),
      longitude: zoneReference.longitude ?? throwError(404, "parameter longitude is missing"),
    },
    avgDCI: 0,
    nSamples: 0,
    cells: [],
    name: zoneName,
  };
}

function calculateVertexes(center: GeoCoordinates): GeoCoordinates[] {
  const offsetLat = cellLength.latitude / 2;
  const offsetLon = cellLength.longitude / 2;
  // latitude ~ y; longitude ~ x
  const v1: GeoCoordinates = {
    latitude: center.latitude + offsetLat,
    longitude: center.longitude + offsetLon,
  };
  const v2: GeoCoordinates = {
    latitude: center.latitude - offsetLat,
    longitude: center.longitude + offsetLon,
  };
  const v3: GeoCoordinates = {
    latitude: center.latitude - offsetLat,
    longitude: center.longitude - offsetLon,
  };
  const v4: GeoCoordinates = {
    latitude: center.latitude + offsetLat,
    longitude: center.longitude - offsetLon,
  };
  return [v1, v2, v3, v4];
}

function emptyCellDoc(cid: CellID, ccenter: GeoCoordinates): CellDoc {
  const vertexes: GeoCoordinates[] = calculateVertexes(ccenter);
  return {
    center: ccenter,
    avgDCI: 0,
    nSamples: 0,
    cellID: cid2str(cid),
    bbox: vertexes,
  };
}

function changeReference(point: GeoCoordinates, newReference: GeoCoordinates): GeoCoordinates {
  return {
    latitude: point.latitude - newReference.latitude,
    longitude: point.longitude - newReference.longitude,
  }; // vp - vref
}

function revertReference(pointFromReference: GeoCoordinates, reference: GeoCoordinates): GeoCoordinates {
  return {
    latitude: reference.latitude + pointFromReference.latitude,
    longitude: reference.longitude + pointFromReference.longitude,
  }; // vpfref + vref
}

function relativeCentroidPos(point: GeoCoordinates, cellLength: GeoCoordinates): [GeoCoordinates, CellID] {
  const stepsLat = Math.ceil(Math.abs(point.latitude) / cellLength.latitude);
  const stepsLon = Math.ceil(Math.abs(point.longitude) / cellLength.longitude);

  const lat = Math.sign(point.latitude) * (stepsLat * cellLength.latitude - cellLength.latitude / 2);
  const lon = Math.sign(point.longitude) * (stepsLon * cellLength.longitude - cellLength.longitude / 2);
  return [
    { latitude: lat, longitude: lon },
    { pLat: Math.sign(point.latitude) * stepsLat, pLon: Math.sign(point.longitude) * stepsLon },
  ];
}

function relativeCentroid(point: GeoCoordinates, reference: GeoCoordinates, cellLength: GeoCoordinates): [GeoCoordinates, CellID] {
  // calculate relative geo coordinates of point w.r.t. reference
  const pRelative = changeReference(point, reference);

  // calculate centroid's geo coordinates coordinates
  const [centroidRel, centroidID] = relativeCentroidPos(pRelative, cellLength);
  const centroid = revertReference(centroidRel, reference);

  // return centroid and its step-based id
  return [centroid, centroidID];
}

function cid2str(cid: CellID): string {
  return `${cid.pLat};${cid.pLon}`;
}

export { emptyCityDoc, emptyZoneDoc, relativeCentroid, cid2str, emptyCellDoc };
