import { useEffect, useState } from "react";

import { HouseList } from "../house/house-list.component";

export const MainPage = () => {
  let data = [];
  const [ allHouses, setAllHouses ] = useState([]);
  useEffect(() => {
    (async () => {
      const response = await fetch("http://localhost:9500/houses"); // TODO have a config for this
      data = await response.json();
      setAllHouses(data);
    })();
  }, []);
  return (
  	<HouseList houses={allHouses}/>
  );
};
