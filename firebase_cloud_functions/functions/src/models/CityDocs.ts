import { GeoCoordinates } from "./GeoTypes";

type CellDoc = {
  center: GeoCoordinates;
  avgDCI: number;
  nSamples: number;
  cellID: string;
  bbox: GeoCoordinates[]; // cubic
};

type ZoneDoc = {
  center: GeoCoordinates;
  avgDCI: number;
  cells: CellDoc[];
  name: string;
  nSamples: number;
};

type CityDoc = {
  center: GeoCoordinates;
  avgDCI: number;
  zones: ZoneDoc[];
  nSamples: number;
};

export { CityDoc, ZoneDoc, CellDoc };
