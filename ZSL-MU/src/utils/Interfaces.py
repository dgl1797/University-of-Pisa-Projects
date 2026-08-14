import os
from numpy import ndarray, save as npysave, load as npyload

class EvaluationBase():
    loaded: bool = False
    
    def get_state(self) -> ndarray: pass
    def set_state(self, state: ndarray) -> None: pass
    def todict(self) -> dict[str, float]: pass
    def __call__(self, *args, **kwargs) -> float: pass
    
    def load(self, location) -> ndarray:
        self.set_state(npyload(f"{location}.npy"))
        return self.get_state()
    
    def save(self, location: str) -> None:
        if os.path.exists(f"{location}.npy"): os.remove(f"{location}.npy")
        return npysave(f"{location}.npy", self.get_state())