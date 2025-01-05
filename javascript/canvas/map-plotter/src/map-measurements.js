/**
 * Utility to calculate distance from point to anchor.
 */
export const calculateDistance = ({ anchor, point }) => {
  const dx = Math.abs(point.x - anchor.x);
  const dy = Math.abs(point.y - anchor.y);
  return Math.sqrt(dx * dx + dy * dy);
};

/**
 * Utility to calculate bearing from point to anchor,
 * assuming x=0 and y=0 is in the top-left corner.
 */
export const calculateBearing = ({ anchor, point }) => {
  const angle = Math.atan2(point.x - anchor.x, anchor.y - point.y) * (180 / Math.PI);
  return (angle + 360) % 360; // Ensure the bearing is within 0-359º
}

