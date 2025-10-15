export interface StatState {
  status: string;
  givenLikes: number;
  numberOfPrivate: number;
  numberOfPublic: number;
  popularityScore: number;
  bestContributor: {
    username: string;
    contribution: number;
  };
}
