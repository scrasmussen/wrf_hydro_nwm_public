#!/usr/bin/env python3

import argparse
import math
import pathlib
import re
import shutil
import subprocess
import tempfile


def run_text(command):
    return subprocess.check_output(command, text=True)


def read_values(path, variable):
    output = run_text([
        "ncks",
        "-H",
        "-C",
        "-s",
        "%.17g\n",
        "-v",
        variable,
        str(path),
    ])
    return [float(value) for value in output.split()]


def read_attribute(header, name):
    match = re.search(
        rf"(?:^|\n)\s*(?:[A-Za-z0-9_]+:|:){re.escape(name)}\s*=\s*"
        rf"([-+0-9.eEdD]+)",
        header,
    )
    if match is None:
        raise ValueError(f"Required projection attribute {name!r} was not found")
    return float(match.group(1).replace("D", "E").replace("d", "e"))


def coordinate_edges(centers):
    if len(centers) < 2:
        raise ValueError("At least two coordinate centers are required")
    edges = [centers[0] - 0.5 * (centers[1] - centers[0])]
    edges.extend(0.5 * (left + right) for left, right in zip(centers, centers[1:]))
    edges.append(centers[-1] + 0.5 * (centers[-1] - centers[-2]))
    return edges


def lambert_inverse_factory(header):
    degrees_to_radians = math.pi / 180.0
    radius = read_attribute(header, "earth_radius")
    standard_parallel_1 = read_attribute(header, "TRUELAT1") * degrees_to_radians
    standard_parallel_2 = read_attribute(header, "TRUELAT2") * degrees_to_radians
    latitude_origin = read_attribute(header, "latitude_of_projection_origin") * degrees_to_radians
    longitude_origin = read_attribute(header, "longitude_of_central_meridian") * degrees_to_radians
    false_easting = read_attribute(header, "false_easting")
    false_northing = read_attribute(header, "false_northing")

    if math.isclose(standard_parallel_1, standard_parallel_2):
        cone = math.sin(standard_parallel_1)
    else:
        cone = math.log(
            math.cos(standard_parallel_1) / math.cos(standard_parallel_2)
        ) / math.log(
            math.tan(math.pi / 4.0 + standard_parallel_2 / 2.0)
            / math.tan(math.pi / 4.0 + standard_parallel_1 / 2.0)
        )

    scale = (
        math.cos(standard_parallel_1)
        * math.tan(math.pi / 4.0 + standard_parallel_1 / 2.0) ** cone
        / cone
    )
    rho_origin = (
        radius
        * scale
        / math.tan(math.pi / 4.0 + latitude_origin / 2.0) ** cone
    )

    def inverse(projected_x, projected_y):
        projected_x -= false_easting
        projected_y -= false_northing
        rho = math.hypot(projected_x, rho_origin - projected_y)
        theta = math.atan2(projected_x, rho_origin - projected_y)
        latitude = (
            2.0 * math.atan((radius * scale / rho) ** (1.0 / cone))
            - math.pi / 2.0
        ) / degrees_to_radians
        longitude = (longitude_origin + theta / cone) / degrees_to_radians
        return latitude, longitude

    return inverse


def write_values(handle, name, values, values_per_line=6):
    handle.write(f" {name} =\n  ")
    for index, value in enumerate(values):
        handle.write(f"{value:.15g}")
        if index == len(values) - 1:
            handle.write(";")
        else:
            handle.write(", ")
        if (index + 1) % values_per_line == 0 and index != len(values) - 1:
            handle.write("\n  ")
    handle.write("\n")


def create_scrip(input_path, output_path, keep_cdl=False):
    header = run_text(["ncdump", "-h", str(input_path)])
    x_centers = read_values(input_path, "x")
    y_centers = read_values(input_path, "y")
    center_latitudes = read_values(input_path, "LATITUDE")
    center_longitudes = read_values(input_path, "LONGITUDE")
    grid_size = len(x_centers) * len(y_centers)

    if len(center_latitudes) != grid_size or len(center_longitudes) != grid_size:
        raise ValueError("LATITUDE/LONGITUDE dimensions do not match x and y")

    inverse_lambert = lambert_inverse_factory(header)
    x_edges = coordinate_edges(x_centers)
    y_edges = coordinate_edges(y_centers)
    corner_latitudes = []
    corner_longitudes = []

    for y_index in range(len(y_centers)):
        south = min(y_edges[y_index], y_edges[y_index + 1])
        north = max(y_edges[y_index], y_edges[y_index + 1])
        for x_index in range(len(x_centers)):
            west = min(x_edges[x_index], x_edges[x_index + 1])
            east = max(x_edges[x_index], x_edges[x_index + 1])
            for projected_x, projected_y in (
                (west, south),
                (east, south),
                (east, north),
                (west, north),
            ):
                latitude, longitude = inverse_lambert(projected_x, projected_y)
                corner_latitudes.append(latitude)
                corner_longitudes.append(longitude)

    output_path.parent.mkdir(parents=True, exist_ok=True)
    temporary_cdl = tempfile.NamedTemporaryFile(
        mode="w",
        suffix=".cdl",
        prefix="fulldom_scrip_",
        delete=False,
    )
    cdl_path = pathlib.Path(temporary_cdl.name)

    try:
        with temporary_cdl as handle:
            handle.write(
                "netcdf Fulldom_hires_scrip {\n"
                "dimensions:\n"
                f" grid_size = {grid_size};\n"
                " grid_corners = 4;\n"
                " grid_rank = 2;\n"
                "variables:\n"
                " int grid_dims(grid_rank);\n"
                " double grid_center_lat(grid_size);\n"
                '  grid_center_lat:units = "degrees";\n'
                " double grid_center_lon(grid_size);\n"
                '  grid_center_lon:units = "degrees";\n'
                " double grid_corner_lat(grid_size, grid_corners);\n"
                '  grid_corner_lat:units = "degrees";\n'
                " double grid_corner_lon(grid_size, grid_corners);\n"
                '  grid_corner_lon:units = "degrees";\n'
                " int grid_imask(grid_size);\n"
                "data:\n"
                f" grid_dims = {len(x_centers)}, {len(y_centers)};\n"
            )
            write_values(handle, "grid_center_lat", center_latitudes)
            write_values(handle, "grid_center_lon", center_longitudes)
            write_values(handle, "grid_corner_lat", corner_latitudes)
            write_values(handle, "grid_corner_lon", corner_longitudes)
            write_values(handle, "grid_imask", [1] * grid_size, values_per_line=20)
            handle.write("}\n")

        subprocess.run(
            ["ncgen", "-4", "-o", str(output_path), str(cdl_path)],
            check=True,
        )
        if keep_cdl:
            saved_cdl = output_path.with_suffix(output_path.suffix + ".cdl")
            shutil.copyfile(cdl_path, saved_cdl)
    finally:
        cdl_path.unlink(missing_ok=True)


def main():
    parser = argparse.ArgumentParser(
        description="Create a SCRIP grid from a Lambert WRF-Hydro Fulldom file"
    )
    parser.add_argument("input", type=pathlib.Path)
    parser.add_argument("output", type=pathlib.Path)
    parser.add_argument("--keep-cdl", action="store_true")
    arguments = parser.parse_args()

    for command in ("ncks", "ncdump", "ncgen"):
        if shutil.which(command) is None:
            raise RuntimeError(f"Required command {command!r} was not found")

    create_scrip(arguments.input, arguments.output, arguments.keep_cdl)


if __name__ == "__main__":
    main()
