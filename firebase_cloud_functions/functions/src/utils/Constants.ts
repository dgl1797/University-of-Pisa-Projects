import { GeoCoordinates } from "../models/GeoTypes";

function makeCellLength(kmLat: number, kmLon: number): GeoCoordinates {
  const R = 6371; // Kilometers
  return {
    latitude: ((kmLat / R) * 180) / Math.PI,
    longitude: ((kmLon / R) * 180) / Math.PI,
  };
}

const cellLength: GeoCoordinates = makeCellLength(0.25, 0.25);

export { cellLength };
