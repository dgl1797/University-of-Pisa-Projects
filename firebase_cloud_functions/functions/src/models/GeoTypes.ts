type GeoPointWithDCI = {
  point: {
    mAltitude: number;
    mLatitude: number;
    mLongitude: number;
  };
  dci: number;
};

type GeoCoordinates = { latitude: number; longitude: number };
type CellID = { pLat: number; pLon: number };

export { GeoCoordinates, GeoPointWithDCI, CellID };
