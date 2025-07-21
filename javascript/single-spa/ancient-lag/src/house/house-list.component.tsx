import { HouseBasicInfo, House } from '../../../acme-backend/src/dto/house';

export const HouseListItem = ({ house }: { house: House }) => {
	return (
		<tr className="table-row">
			<td>{house.address.line1}</td>
			<td>{house.price}</td>
		</tr>
	);
};

export const HouseList = ({ houses }: { houses: HouseBasicInfo[] }) => {
	return (
		<div className="limited-width-box">
			<h4>Houses</h4>
			<table className="table">
				<tbody>
					{houses.map(h => (
						<HouseListItem key={h.id} house={h} />
					))}
				</tbody>
			</table>
		</div>
	);
};
