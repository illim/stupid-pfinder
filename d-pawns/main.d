import std.stdio;
import std.algorithm: canFind, sort;
import std.conv;

struct Point {
    int x;
    int y;
}

struct World {
    Point[] entities;
}

const int WORLD_SIZE = 10;

void main() {
    auto world = initWorld();
    Point orig = { 3, 3 };
    Point dest = { 8, 6 };
    auto finder = new StupidFinder(world);
    auto results = finder.find(orig, dest);
    foreach(result; results) {
        writeln("result:", result);
        writeln(result.path.toString());
    }
}

bool isClean(World world, Point p) {
    return p.x > -1 && p.y > -1
    && p.x < WORLD_SIZE && p.y < WORLD_SIZE
    && ! world.entities.canFind(p);
}

Point add(Point p, Point p2) {
    return Point(p.x + p2.x, p.y + p2.y);
}

World initWorld() {
     auto entities = new Point[6];
     for(int i=0; i<entities.length; i++) {
       entities[i] = Point(7, i + 3);
     }
     return World(entities);
}


alias StepCosts = double[Point];

struct Path {
    Point value;
    Path* next;
}

string toString(Path* path) {
    if (path.next) {
        return "(" ~ to!string(path.value) ~ "," ~ ((*path).next).toString() ~ ")";
    } else {
        return "(" ~ to!string(path.value) ~ ")";
    }
}

struct SearchResult {
    Path* path;
    double cost;
}

class StupidFinder {
    StepCosts stepCosts;
    double[Point] marks;
    World world;

    this(World world) {
        this.world = world;
        stepCosts = getStepCosts();
    }

    SearchResult[] find(Point orig, Point dest) {
        auto start = new Path(orig, null);
        return bestDist(orig, dest, start, 0);
    }

    SearchResult[] bestDist(Point p, Point dest, Path* path, double accCost) {
        if (p == dest){
            return [SearchResult(path, accCost)];
        } else {
            auto nextSteps = new Point[0];
            foreach(step, stepCost; stepCosts) {
                auto next = p.add(step);
                auto oldCost = marks.get(next, 0);
                auto cost = accCost + stepCost;
                if (world.isClean(next) && (oldCost == 0 || oldCost > cost)) {
                    marks[next] = cost;
                    nextSteps ~= next;
                }
            }
            auto searchResults = new SearchResult[0];
            foreach(next; nextSteps){
                auto newPath = new Path(next, path);
                auto res = bestDist(next, dest, newPath, marks[next]);
                if (res.length > 0) {
                    searchResults ~= res;
                }
            }
            if (searchResults.length == 0) {
                return [];
            } else {
                searchResults.sort!((x, y) => x.cost < y.cost);
                return [searchResults[0]];
            }
        }
    }

    StepCosts getStepCosts() {
        StepCosts result;
        foreach(e; [Point(1, 0), Point(0, 1), Point(0, -1), Point(-1, 0)]){
            result[e] = 1.0;
        }
        foreach(e; [Point(1, 1) , Point(1, -1) , Point(-1, 1) , Point(-1, -1)]){
            result[e] = 1.414213;
        }
        return result;
    }

}
